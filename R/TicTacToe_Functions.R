check_win=function(board,WS){
  W1=W2=c(rep(0,9))
  for(i in 1:8){
    W1[i]=all(board[WS[[i]]]==1)
    W2[i]=all(board[WS[[i]]]==2)
  }
  if(sum(W1)<1 && sum(W2)<1){
    win = 0
  }else{
    if(sum(W1)>=1){
      win = 1
    }
    if(sum(W2)>=1){
      win = 2
    }
  }
  return(win)
}


get_square = function(V){

  #Square 2
  if(3<V[1]&V[1]<=6 && 9>V[2]&V[2]>=6){
    P = list(2,c(ceiling(9-V[2]),ceiling(V[1]-3)))
  }

  #Square 1
  if(0<=V[1]&V[1]<=3 && 9>V[2]&V[2]>=6){
    P = list(1,c(ceiling(9-V[2]),ceiling(V[1])))
  }

  #Square 3
  if(6<V[1]&V[1]<=9 && 9>V[2]&V[2]>=6){
    P = list(3,c(ceiling(9-V[2]),ceiling(V[1]-6)))
  }

  #Square 4
  if(V[1]<=3 && 3 <= V[2]&V[2] <6){
    P = list(4,c(ceiling(6-V[2]),ceiling(V[1])))
  }

  #Square 5
  if(3<V[1] &V[1]<=6 && 3 <= V[2]&V[2] < 6){
    P = list(5,c(ceiling(6-V[2]),ceiling(V[1]-3)))
  }

  #Square 6
  if(6<V[1] &V[1]<=9 && 3<=V[2]&V[2] <6){
    P = list(6,c(ceiling(6-V[2]),ceiling(V[1]-6)))
  }

  #Square 7
  if(V[1]<=3 && V[2] <=3){
    P = list(7,c(ceiling(3-V[2]),ceiling(V[1])))
  }

  #Square 8
  if(3<V[1]&V[1]<=6 && V[2] <=3){
    P = list(8,c(ceiling(3-V[2]),ceiling(V[1]-3)))
  }

  #Square 9
  if(6<V[1]&V[1]<=9 && V[2] <=3){
    P = list(9,c(ceiling(3-V[2]),ceiling(V[1]-6)))
  }
  return(P)
}


conv_to_ind = function(move){
  if(move[1]==1){
    return(move[2])
  }
  if(move[1]==2){
    return(move[2]+3)
  }
  if(move[1]==3){
    return(move[2]+6)
  }
}


legal_move = function(board,move){
  P = get_square(move)
  I = conv_to_ind(P[[2]])
  if(!all(board[[I]]==0)){
    return(list(TRUE,I))
  }else{
    return(list(FALSE,I))
  }
}

