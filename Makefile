##
## EPITECH PROJECT, 2018
## 208dowels
## File description:
## Makefile for 208dowels.
##

NAME	=	208dowels

all:
	make $(NAME)

$(NAME):
	stack build
	cp `stack path --local-install-root`/bin/$(NAME) .

.PHONY:	all $(NAME)
