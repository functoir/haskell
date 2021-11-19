/**
 * @file main.c
 * @author siavava <amittaijoel@outlook.com>
 * @brief main functionality for the tic-tac-toe game
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

#define SCREEN_WIDTH 1280
#define SCREEN_HEIGHT 960


int 
main (int argc, const char* argv[]) 
{
  if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
    fprintf(stderr, "SDL_Init Error: %s\n", SDL_GetError());
    return EXIT_FAILURE;
  }

  SDL_Window* window;
  if ( (window = initWindow()) == NULL) {
    fprintf(stderr, "initWindow Error: %s\n", SDL_GetError());
    exit (EXIT_FAILURE);
  }

  SDL_Renderer* renderer;
  if ( (renderer = initRenderer(window)) == NULL) {
    fprintf(stderr, "initRenderer Error: %s\n", SDL_GetError());
    exit (EXIT_FAILURE);
  }

  game_t* game;
  if ( (game = initGame() == NULL)) {
    fprintf(stderr, "initGame Error.\n");
    exit (EXIT_FAILURE);
  }

  SDL_Event event;
  while (game->state != QUIT) {
    while (SDL_PollEvent(&event)) {
      switch (event.type) {
        case SDL_QUIT: {
          game->state = QUIT;
          break;
        }
        case SDL_MOUSEBUTTONDOWN: {
          click_on_cell(game, event.button.x, event.button.y);
          break;
        }
        defalut: { ; }
      }
    }
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    SDL_RenderClear(renderer);
    render(game, renderer);
    SDL_RenderPresent(renderer);
  }


}

SDL_Window* 
initWindow() 
{
  SDL_Window* window = SDL_CreateWindow("Tic Tac Toe",
   100, 1000, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN);
  if (window == NULL) {
    fprintf(stderr, "SDL_CreateWindow Error: %s\n", SDL_GetError());
    return NULL;
  }
  return window;
}

SDL_Renderer*
initRenderer(SDL_Window* window)
{
  SDL_Renderer* renderer = SDL_CreateRenderer(window, -1, 
   SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
}

void
gameOver(SDL_Renderer * renderer,const game_t* game, SDL_Color* color)
{

}