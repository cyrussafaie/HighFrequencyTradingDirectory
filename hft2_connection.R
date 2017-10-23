## hft2_connection.R

############### CONNECTION CODE ####################


# smart way to check we have specific libs installed or install them otherwise
usePackage <- function(p) {
    if (!is.element(p, installed.packages()[,1])) {
        install.packages(p, dep = TRUE);
    }
    require(p, character.only = TRUE);
}

# send protobuf message with 2 bytes of length before
SendProtobufMsg <- function(con, msg) {
    # 0. check if the message is initialized
    if ( !msg$isInitialized() ) {
        stop(paste("Protobuf message not initialized:", msg$toString()));
    }
    # 1. send message length (2 bytes) as a header
    header <- msg$bytesize();
    writeBin(header, con, size = 2, endian = "big");
    # 2. send message
    msg$serialize(con);
    return;
}


# receive raw protobuf message
ReceiveRawMsg <- function(con) {
    # 1. receive next message length as a 2-byte header
    msg_len <- readBin(con, what = "int", size = 2, signed = FALSE, endian = "big");
    # check received msg_len
    if ( length(msg_len) != 1 ) {
        stop(paste("expected to receive 1 integer as a header while received", length(msg_len)))
    }
    if (msg_len > 100000) { # normally message length shouldn't be > 100000 bytes
        stop(paste("received too large msg length: ", msg_len));
    }
    # 2. receive protobuf-message with specific length
    #return ( readBin(con, what = "raw", n = msg_len) ); ## might return <n bytes!
    msg <- raw(0);
    while (length(msg) < msg_len) {
        chunk <- readBin(con, what = "raw", n = msg_len - length(msg));
        if (length(chunk) > 0) {
            msg <- c(msg, chunk);
        }
        else { # length(chunk) <= 0
            stop("Looks like connection is lost...");
        }
    }
    return (msg);
}


# send login request and receive login reply
Authorize <- function(con, login, pwd, stream_name)
{
    # generate login message
    login_msg <- Authentication.LoginRequest$new(login = login, 
                                                   enc_password = pwd,
                                                   stream_name = stream_name);
    # send login message
    message("Sending login message");
    SendProtobufMsg(con, login_msg);
    
    # receive login-reply message and handle it
    raw_msg <- ReceiveRawMsg(con);
    login_reply <- Authentication.LoginReply$read(raw_msg);
    if (login_reply$connection_status != Authentication.LoginReply$LoginErrorsEnum$OK) {
        stop("Login failed: ", name(Authentication.LoginReply$LoginErrorsEnum$value(number = login_reply$connection_status)) );
    }
    
    # now we're logged in
    message("Logged in successfully as ", login);
}


# main function, connects to server and invokes user specified handlers in the event loop
## markettrade_handler is expected to have 4 arguments:
### 1) symbol('ES' or 'VX')
### 2) trade_price (integer vector of unit length)
### 3) trade_size (integer vector of unit length)
### 4) trade_side('A' or 'B')
## and to return 1 to buy@ask, -1 to sell@bid and 0 to do nothing.
## orderexec_handler is expected to have 4 arguments:
### 1) side ('BOUGHT' or 'SOLD')
### 2) quantity (int vector of unit length)
### 3) price (int vector of unit length)
### 4) forced (bool vector of unit length) - TRUE when the order is initiated by the system 
## and to return nothing
Connect <- function(host, port, login, password, stream_name, markettrade_handler, orderexec_handler, catch_handler_errors=TRUE) {
    
    problems_buf_sz <- 10000;
    current_problem_n <- 0;
    result <- list(problems=data.frame(time=.POSIXct(rep(NA, problems_buf_sz)),
                                       problem=character(problems_buf_sz),
                                       stringsAsFactors = FALSE
                                       ),
                   n_signals = 0,
                   pnl = NaN,
                   total_trades = 0,
                   manual_trades = 0, 
                   forced_trades = 0
    );
    
    # connect to server
    message("Connecting to ", host, ":", port);
    con <- socketConnection(host, port, blocking = TRUE, server = FALSE, open="r+b", timeout = 120);
    # end of connection handler:
    on.exit( { close(con);  
               message("Connection closed"); 
               message("You sent total of ", result$n_signals, " signal(s) to server");
             } );
    
    # make authorization
    Authorize(con, login, password, stream_name);
    
    message('Receiving live datastream...')
    
    # event-loop for server messages
    repeat {
        raw_msg <- ReceiveRawMsg(con);
        event_msg <- HFT_2.Event$read(raw_msg);
        # check errors
        if ( nchar(event_msg$error) > 0 ) {
            problem <- paste("SERVER SENT: '", event_msg$error, "'", sep='');
            message(problem);
            current_problem_n <- current_problem_n + 1;
            result$problems[current_problem_n,] <- list(Sys.time(), problem);
        }
        # process message from server
        if ( event_msg$has("market_trade_event") ) {
            # process market trade
            signal <- NULL;
            tryCatch({
                signal <- markettrade_handler(event_msg$market_trade_event$symbol,
                                              event_msg$market_trade_event$trade_price,
                                              event_msg$market_trade_event$trade_size,
                                              event_msg$market_trade_event$trade_side);
                # assert type & length of signal
                stopifnot(is.numeric(signal), length(signal) == 1);
            }, error = function(e) {
                if (catch_handler_errors) {
                    problem <- paste("Error inside markettrade_handler: ", e, 'Forcing signal to 0', sep='');
                    message('!!!***   WARNING   ***!!!\n', 
                            problem,
                            '\n!!!*******************!!!');
                    current_problem_n <<- current_problem_n + 1;
                    result$problems[current_problem_n,] <<- list(Sys.time(), problem);
                    signal <<- 0;
                }
                else {
                    stop(e);
                }
            })
            if (signal != 0) {
                signal_msg <- HFT_2.Signal$new(signal = signal);
                SendProtobufMsg(con, signal_msg);
                result$n_signals <- result$n_signals + 1;
            }
        }
        if ( event_msg$has("order_filled_event") ) {
            # process new order execution
            tryCatch({
                orderexec_handler(ifelse(event_msg$order_filled_event$side == HFT_2.SideEnum$BUY, 'BOUGHT', 'SOLD'),
                                  event_msg$order_filled_event$quantity,
                                  event_msg$order_filled_event$price,
                                  event_msg$order_filled_event$forced);
            }, error = function(e) {
                if (catch_handler_errors) {
                    problem <- paste("Error inside orderexec_handler: ", e, sep='');
                    message('!!!***   WARNING   ***!!!\n', 
                            problem,
                            '\n!!!*******************!!!');
                    current_problem_n <<- current_problem_n + 1;
                    result$problems[current_problem_n,] <<- list(Sys.time(), problem);
                } 
                else {
                    stop(e);
                }
            });
        }
        if ( event_msg$has("stream_end_event") ) {
            # break from repeat-loop in case server stream ends
            message("Stream has ended, goodbye!");
            result$pnl = event_msg$stream_end_event$pnl;
            result$total_trades = event_msg$stream_end_event$total_trades;
            result$manual_trades = event_msg$stream_end_event$manual_trades;
            result$forced_trades = event_msg$stream_end_event$forced_trades;
            message("Some statistics:");
            message("PnL=", result$pnl);
            message("Total trades: ", result$total_trades);
            message("Your trades: ", result$manual_trades);
            message("Forced trades: ", result$forced_trades);
            result$problems <- result$problems[!is.na(result$problems$time),];
            return(result)
        }
    }
}

# POSIXct time with fractional seconds:
options(digits.secs = 6)
# parse proto-files
usePackage("RProtoBuf");
readProtoFiles(dir="./");    # read all proto-files from current folder
message("hft2_connection.R sourced!");

############# CONNECTION CODE END ##################