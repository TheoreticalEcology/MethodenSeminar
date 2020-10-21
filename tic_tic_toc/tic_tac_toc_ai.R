library(tensorflow)
library(keras)
library(Rcpp)
code = "
int evaluate_win_cpp(IntegerVector board){
  int winner = 0;
  int tmp0 = 0; 
  int tmp1 = 0; 
  int tmp2 = 0; 
  
  int zero = -1;
  for(int i = 0; i < 9; i++) {
    if(board(i) == 0) {
      winner = zero;
      break;
    }
  }
  
  
  for(int i = 0;i<3;i++) {
      tmp0 = i*3;
      tmp1 = i*3 + 1;
      tmp2 = i*3 + 2;
    if(board(tmp0) > 0.5) {
      if( board(tmp0) == board(tmp1)) {
        if(board(tmp0) == board(tmp2)) {
          return board(tmp0);
        }
      }

    }
  }
  for(int i = 0;i<3;i++) {
      tmp0 = i;
      tmp1 = i+3;
      tmp2 = i+6;
    if(board(tmp0) > 0.5) {
      if( board(tmp0) == board(tmp1)) {
        if(board(tmp0) == board(tmp2)) {
          return board(tmp0);
        }
      }

    }
  }
  
  if(board(0) > 0.5) {
    if(board(0) == board(4)) {
      if(board(0) == board(8)) return board(0);
    }
  }
  
  if(board(2) > 0.5) {
    if(board(2) == board(4)) {
      if(board(2) == board(6)) return board(0);
    }
  }
  
  
  return winner;
}
"
evaluate_win2 = Rcpp::cppFunction(code)

make_move <- function(board, player, move){
  if(board[move] == 0){
    board[move] <- player
  }else{
    print("illegal move")
  }
  return(board)
}

# Get all possible moves
possible_moves <- function(board){
  moves <- which(board == 0)
  return(moves)
}

# Decide on a random next move
random_move <- function(board){
  if(length(possible_moves(board)) >1){
    random_move <- sample(possible_moves(board), 1)
  }else{
    random_move <- possible_moves(board)
  }
  return(random_move)
}



keras = tf$keras
create_agent = function() {
  m = keras$Sequential(list(
    keras$layers$InputLayer(input_shape = c(10L)),
    keras$layers$Dropout(rate = 0.2), 
    keras$layers$Dense(units = 35L,activation = keras$activations$relu),
    keras$layers$Dropout(rate = 0.2), 
    keras$layers$Dense(units = 35L,activation = keras$activations$relu),
    keras$layers$Dropout(rate = 0.2), 
    keras$layers$Dense(units = 35L,activation = keras$activations$relu),
    keras$layers$Dropout(rate = 0.2), 
    keras$layers$Dense(units = 12L)
  ))
  return(m)
}

loss_func = function(XTm, YTm, m) {
  with(tf$GradientTape() %as% tape, {
    pred = m(XTm)
    loss1 = tf$reduce_mean(keras$losses$categorical_crossentropy(YTm[,1:9,drop=FALSE],  tf$math$softmax(pred[,1:9], axis = 0L)))
    loss2 = tf$reduce_mean(keras$losses$categorical_crossentropy(YTm[,10:12,drop=FALSE],  tf$math$softmax(pred[,10:12], axis = 0L)))
    loss = loss1 + loss2
  })
  grads = tape$gradient(loss, m$weights)
  return(grads)
}

loss_func_tf = tf_function(loss_func)

train_agent = function(m, memory, opt, epoch = 20L) {
  YT = memory[, 11:12, drop=FALSE]+0.00001-1
  XT = memory[,1:10, drop=FALSE]
  for(i in 1:epoch) {
    ind = sample.int(nrow(XT), size = ceiling(0.1*nrow(XT)))
    
    YTm1 = tf$reshape(tf$squeeze(k_one_hot(YT[ind, 11,drop=FALSE]-1, 9)), list(-1L, 9L))
    YTm2 = tf$reshape(tf$squeeze(k_one_hot(YT[ind, 12,drop=FALSE]-1, 3)), list(-1L, 3L))
    YTm = tf$concat(list(YTm1, YTm2), axis = 1L)
    XTm = XT[ind, ,drop=FALSE]
    # with(tf$GradientTape() %as% tape, {
    #   pred = m(XTm)
    #   loss = tf$reduce_mean(keras$losses$categorical_crossentropy(YTm,  pred))
    # })
    # grads = tape$gradient(loss, m$weights)
    grads = loss_func_tf(XTm, YTm, m)
    opt$apply_gradients(purrr::transpose(list(grads, m$weights)))
  }
}

simulate_game <- function(board, player_w){
  
  boards_history <- data.frame(matrix(NA, ncol = 9))
  #boards_history <- data.frame(X1 = numeric(), X2=numeric)
  
  winner = -1
  player = player_w
  
  while(winner < 0){
    
    next_move <- random_move(board)
    board <- make_move(board, player, next_move)
    if(player == 1){player = 2}else{player = 1}
    winner <- evaluate_win2(board)
    #print(board)
  }
  return(winner)
}


game <- function(ai_on = T, ai_mode = "aggressive"){
  
  
  # new agent
  
  memory = matrix(NA, 400, 9+1+1+1+1)
  counter = 1
  
  for(games in 1:500){
    board <-  c(0,0,0,
                0,0,0,
                0,0,0)
    winner = -1
    agent = create_agent()
    opt = keras$optimizers$RMSprop(learning_rate = 0.01)
    
    ### training ###
    sub_memory = memory[complete.cases(memory), ]
    
    if(nrow(sub_memory) > 0) {
      train_agent(agent, sub_memory, opt)
    } 
    while(winner < 0) {
      
      k = counter
      player = 1
      memory[k, 1:9] = board
      pred = agent(cbind(matrix(board, 1), player))$numpy()
      next_move_p = tf$math$softmax(pred[1,1:9,drop=FALSE])$numpy()
      next_win =  tf$math$softmax(pred[1,10:12,drop=FALSE])$numpy()
      while(TRUE) {
        next_move = sample(1:9, 1, prob = scales::rescale(next_move_p)+0.2)
        if(next_move %in% possible_moves(board)) break()
      }
      memory[k, 10] = player
      memory[k, 11] = which.max(next_move_p)#next_move
      
      # board update
      pm = possible_moves(board)
      
      
      

      
      old = board
      results =
        sapply(1:100, function(j) {
          sapply(pm, function(i) {
            board_test<- make_move(old, player, i)
            eval_test = evaluate_win2(board_test)
            if(eval_test > -0.1){
              if(eval_test == player) return(player)
              else return(0)
            }
            result_test = simulate_game(board_test, player)
            return(result_test)
          })
        })
      
      best_possible = which.max(apply(t(results), 2, sum))
      
      
      memory[k, 12] = simulate_game(board, 0)
      board<- make_move(board, player, next_move)
      memory[k, 12] = simulate_game(board, 0)
      memory[k, 12] = best_possible
      winner = evaluate_win2(board)
      

      
      if(winner>-0.1) break()
      
      counter = counter + 1
      
      k = counter
      
      player = 2
      #memory[k, 1:9] = board
      #next_move_p = agent(cbind(matrix(board, 1), player))$numpy()
      while(TRUE) {
        next_move  = random_move(board)
       # next_move = sample(1:9, 1, prob = scales::rescale(next_move_p)+0.2)
        if(next_move %in% possible_moves(board)) break()
      }
      #memory[k, 10] = player
      #memory[k, 11] = which.max(next_move_p)
      
      # board update
      #pm = possible_moves(board)
      
      # results = 
      #   sapply(1:100, function(j) {
      #     sapply(pm, function(i) {
      #       board_test<- make_move(board, player, i)
      #       eval_test = evaluate_win2(board_test)
      #       if(eval_test > -0.1){
      #         if(eval_test == player) return(TRUE)
      #         else return(FALSE)
      #       }
      #       result_test = simulate_game(board_test, player)
      #       return(result_test)
      #     })
      #   })
      # 
      #best_possible = which.max(apply(t(results), 2, sum))
      board<- make_move(board, player, next_move)
      #memory[k, 12] = best_possible
      #winner = evaluate_win2(board)

      #counter = counter+1
      
      if(counter > 10) train_agent(agent, memory[complete.cases(memory), ], opt, epoch = max(counter, 50))
    }
    cat("Winner: ", winner, "in game: ", games, " Score: ", mean(memory[complete.cases(memory),9] == memory[complete.cases(memory),10])," \n")
    
    if(counter > 100) counter = 1
  }
  
}



simulate_game <- function(board, player_w){
  
  winner = NULL
  player = player_w
  
  while(is.null(winner)){
    
    next_move <- random_move(board)
    board <- make_move(board, player, next_move)
    if(player == 1){player = 2}else{player = 1}
    winner <- evaluate_win(board)
    #print(board)
  }
  return(winner==player_w)
}


library(Rcpp)
code = "
int simulate_game(NumericVector board, int player_w) {
  
  int winner = -1;
  
  return player_w;

}"

simulate_game_cpp = Rcpp::cppFunction(code)
