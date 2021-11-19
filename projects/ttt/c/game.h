/**
 * @file game.h
 * @author siavava <amittaijoel@outlook.com>
 * @brief module to handle tic-tac-toe gamestate
 * @version 0.1
 * @date 2021-08-02
 * 
 * @copyright Copyright (c) 2021
 * 
 */

#ifndef _GAME_H_
#define _GAME_H_

#define N 3
#define SCREEN_WIDTH 1280
#define SCREEN_HEIGHT 960

#define ACTIVE 0
#define X_WIN 1
#define O_WIN 2
#define TIE 3
#define QUIT 4

#define PLAYER_X 101
#define PLAYER_O 102

#define EMPTY -1
#define IS_X -2
#define IS_O -3

typedef struct game {
  int** board;
  int turn;
  int state;
} game_t;

game_t* game_init();

#endif /* _GAME_H_ */