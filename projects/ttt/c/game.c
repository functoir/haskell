/**
 * @file game.c
 * @author core functionality of game module
 * @brief 
 * @version 0.1
 * @date 2021-08-02
 * 
 * @copyright Copyright (c) 2021
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include "game.h"

game_t*
game_init(void)
{
  game_t* game = malloc(sizeof(game_t));
  if (game == NULL) {
    fprintf(stderr, "ERROR: game_init: malloc failed\n");
    return NULL;
  }

  game->board = calloc(N, sizeof(int*));
  if (game->board == NULL) {
    fprintf(stderr, "ERROR: game_init: calloc failed\n");
    free(game);
    return NULL;
  } else {
    for (int i = 0; i < N; i++) {
      game->board[i] = calloc(N, sizeof(int));
      for (int j = 0; j < N; j++) {
        game->board[i][j] = EMPTY;
      }
    }
  }

  game->turn = PLAYER_X;
  game->state = ACTIVE;

  return game;
}
