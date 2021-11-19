/**
 * @file rendering.h
 * @author siavava <amittaijoel@outlook.com>
 * @brief Handler for rendering functionality.
 * @version 0.1
 * @date 2021-08-02
 * 
 * @copyright Copyright (c) 2021
 * 
 */

#ifndef __RENDERING_H_
#define __RENDERING_H_

#include <stdio.h>
#include <stdlib.h>
#include <SDL2/SDL.h>
#include "game.h"

SDL_Renderer* initRenderer(SDL_Window* window);

void render(SDL_Renderer* renderer, const game_t* game);

#endif /* __RENDERING_H_ */