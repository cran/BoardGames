#' Play some Ultimate Tic-Tac-Toe?
#'
#' This function allows one to play the Ultimate version of Tic-Tac-Toe.
#' In the Regular version of Tic-Tac-Toe, players take turns placing their marks, with the objective of achieving three marks in a row in any direction.
#' 9x9 Tic-Tac-Toe or more commonly known as Ultimate Tic-Tac-Toe, adds a twist on the regular version of
#' Tic-Tac-Toe that most of us have come to know. Perceive the board as a big Tic-Tac-Toe board, with
#' the goal being to achieve 3 big marks in any direction. Big marks are achieved by winning the corresponding
#' small Tic-Tac-Toe blocks. The player to move first may play anywhere on the board. However, following moves
#' must correspond to the same big Tic-Tac-Toe block of the small Tic-Tac-Toe board where the last move was played.
#' @keywords tictactoe tic tac toe ultimate connect row dataframe matrix game fun
#' @importFrom graphics rect segments abline locator plot points
#' @importFrom grDevices adjustcolor dev.new dev.next
#' @export

UltimateTicTacToe = function(){
  if(names(dev.next())=="null device"){
    dev.new()
    dev.new()
  }else{
    dev.new()
  }
  n=18
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
       ylab = "", bty = "o", lab = c(n, n, 1))
  segments(0,c(6,12),n,c(6,12),lwd=3)
  segments(c(6,12),0,c(6,12),n,lwd=3)
  segments(0,c(2,4,8,10,14,16),n,c(2,4,8,10,14,16),lwd=1)
  segments(c(2,4,8,10,14,16),0,c(2,4,8,10,14,16),n,lwd=1)

  red_fade  = adjustcolor("red" , alpha.f = 0.2)
  blue_fade = adjustcolor("blue", alpha.f = 0.2)
  played_moves=NULL
  turn=1
  next_move = c(1:9)
  board99 = rep(list(rep(0,9)),9)
  current_status = rep(0,9)
  bg_ind = 1:9
  bs_ind = 1:9

  big_ind = list(c(3,15), c(9,15), c(15,15),
                 c(3,9),  c(9,9),  c(15,9),
                 c(3,3),  c(9,3),  c(15,3))

  CS      = list(c(0,12,6,18), c(6,12,12,18), c(12,12,18,18),
                 c(0,6,6,12),  c(6,6,12,12),  c(12,6,18,12),
                 c(0,0,6,6),   c(6,0,12,6),   c(12,0,18,6))

  winning_seq = list(c(1,2,3),
                     c(4,5,6),
                     c(7,8,9),
                     c(1,4,7),
                     c(2,5,8),
                     c(3,6,9),
                     c(1,5,9),
                     c(3,5,7))

  repeat {
    for (j in 1:2) {
      repeat {
        P   = locator(1)
        P$x = round(P$x/2-0.5)+0.5
        P$y = round(P$y/2-0.5)+0.5
        xy  = paste(P, collapse = ":")
        V   = get_square(c(P$x,P$y))
        if (!is.element(xy, played_moves) && is.element(V[[1]],next_move))
          break
      }

      played_moves = c(played_moves, xy)
      points(P$x*2,P$y*2,col=c("red","blue")[turn%%2+1],pch=c(1,4)[turn%%2+1],cex=4,lwd=3)
      segments(0,c(6,12),n,c(6,12),lwd=3)
      segments(c(6,12),0,c(6,12),n,lwd=3)
      abline(h=18,lwd=3,col="white")
      abline(h=0,lwd=3,col="white")
      abline(v=18,lwd=3,col="white")
      abline(v=0,lwd=3,col="white")
      next_move = conv_to_ind(V[[2]])
      I  = CS[[next_move]]
      rect(I[1],I[2],I[3],I[4],border="green",lwd=3)
      board99[[V[[1]]]][next_move] = turn%%2 + 1

      for(i in bg_ind){
        A = check_win(board99[[i]],winning_seq)
        if(A != 0){
          current_status[i] = A
          bg_ind = bg_ind[-which(bg_ind==i)]
        }
      }

      for(i in bs_ind){
        CSB = current_status[i]
        if(CSB!=0){
          points(big_ind[[i]][1],big_ind[[i]][2],col=list(red_fade,blue_fade)[[CSB]],pch=c(1,4)[CSB],cex=20,lwd=12)
          bs_ind=bs_ind[-which(bs_ind==i)]
        }
      }
      turn = turn + 1
      if (check_win(current_status,winning_seq)==1 || check_win(current_status,winning_seq)==2) break
    }
    if (check_win(current_status,winning_seq)==1 || check_win(current_status,winning_seq)==2) break
  }
}

