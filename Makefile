SRCS =	main.hs \
		Transition.hs Machine.hs \

OBJS = ${SRCS:.hs=.o}
IBJS = ${SRCS:.hs=.hi}

NAME =	ft_turing

all :		${NAME}

${NAME} :	
			ghc -o ${NAME} main.hs

clean :
			rm -f ${OBJS} ${IBJS}

fclean :	clean
			rm -f ${NAME}

re :		fclean all
