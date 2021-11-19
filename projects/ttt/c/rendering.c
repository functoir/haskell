/**
 * @file rendering.c
 * @author siavava <amittaijoel@outlook.com>
 * @brief Handler for rendering functionality.
 * @version 0.1
 * @date 2021-08-02
 * 
 * @copyright Copyright (c) 2021
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <SDL2/SDL.h>
#include "game.h"
#include "rendering.h"

/* local constants */
const SDL_Color GRID_COLOR = {.r = 255, .g = 255, .b = 255, .a = 255};
const SDL_Color X_WINS_COLOR = {.r = 255, .g = 255, .b = 255, .a = 255};
const SDL_Color O_WINS_COLOR = {.r = 50, .g = 100, .b = 255, .a = 255};
const SDL_Color TIE_COLOR = {.r = 100, .g = 100, .b = 100, .a = 255};

SDL_Renderer*
initRenderer(SDL_Window* window)
{
  SDL_Renderer* renderer = SDL_CreateRenderer(window, -1, 
   SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
}

void
render(SDL_Renderer* renderer, const game_t* game)
{
  switch (game->state) {
    case ACTIVE: {
      render_runningState(renderer, game);
      break;
    }
    case X_WIN: {
      render_gameOverState(renderer, game, &X_WINS_COLOR);
      break;
    }
    case O_WIN: {
      render_gameOverState(renderer, game, &O_WINS_COLOR);
      break;
    }
    case TIE: {
      render_gameOverState(renderer, game, &TIE_COLOR);
      break;
    }
    default: {}

  }
}

void
renderGrid(SDL_Renderer* renderer, const SDL_Color* color)
{
  SDL_SetRenderDrawColor(renderer, color->r, color->g, color->b, color->a);
}

void
renderBoard(SDL_Renderer* renderer, const int** board, const SDL_Color* x_color, const SDL_Color* y_color)
{

}

void
render_runningState(SDL_Renderer* renderer, const game_t* game)
{
  renderGrid(renderer, &GRID_COLOR);
  // renderBoard(renderer, game->board, X_WIN_COLOR );
}

void 
render_gameOverState(SDL_Renderer* renderer, const game_t* game, const SDL_Color* color)
{
  renderGrid(renderer, color);
}
