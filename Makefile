NAME =	project

BINARY_NAME	=	glados

_PATH	:=	$(shell stack path --local-install-root)

all:
	stack build
	cp $(_PATH)/bin/$(NAME)-exe ./$(BINARY_NAME)

clean:
	stack clean

fclean: clean
	rm -f $(_PATH)/bin/$(NAME)-exe
	rm -f $(BINARY_NAME)

re:	fclean all

tests_run:
	stack test --coverage

.PHONY:	all clean fclean re
