# multiAgents.py
# --------------
# Licensing Information:  You are free to use or extend these projects for
# educational purposes provided that (1) you do not distribute or publish
# solutions, (2) you retain this notice, and (3) you provide clear
# attribution to UC Berkeley, including a link to http://ai.berkeley.edu.
# 
# Attribution Information: The Pacman AI projects were developed at UC Berkeley.
# The core projects and autograders were primarily created by John DeNero
# (denero@cs.berkeley.edu) and Dan Klein (klein@cs.berkeley.edu).
# Student side autograding was added by Brad Miller, Nick Hay, and
# Pieter Abbeel (pabbeel@cs.berkeley.edu).


from util import manhattanDistance
from game import Directions
import random, util

from game import Agent

class ReflexAgent(Agent):
    """
    A reflex agent chooses an action at each choice point by examining
    its alternatives via a state evaluation function.

    The code below is provided as a guide.  You are welcome to change
    it in any way you see fit, so long as you don't touch our method
    headers.
    """


    def getAction(self, gameState):
        """
        You do not need to change this method, but you're welcome to.

        getAction chooses among the best options according to the evaluation function.

        Just like in the previous project, getAction takes a GameState and returns
        some Directions.X for some X in the set {NORTH, SOUTH, WEST, EAST, STOP}
        """
        # Collect legal moves and successor states
        legalMoves = gameState.getLegalActions()

        # Choose one of the best actions
        scores = [self.evaluationFunction(gameState, action) for action in legalMoves]
        bestScore = max(scores)
        bestIndices = [index for index in range(len(scores)) if scores[index] == bestScore]
        chosenIndex = random.choice(bestIndices) # Pick randomly among the best

        "Add more of your code here if you want to"

        return legalMoves[chosenIndex]

    def evaluationFunction(self, currentGameState, action):
        """
        Design a better evaluation function here.

        The evaluation function takes in the current and proposed successor
        GameStates (pacman.py) and returns a number, where higher numbers are better.

        The code below extracts some useful information from the state, like the
        remaining food (newFood) and Pacman position after moving (newPos).
        newScaredTimes holds the number of moves that each ghost will remain
        scared because of Pacman having eaten a power pellet.

        Print out these variables to see what you're getting, then combine them
        to create a masterful evaluation function.
        """
        # Useful information you can extract from a GameState (pacman.py)
        successorGameState = currentGameState.generatePacmanSuccessor(action)
        newPos = successorGameState.getPacmanPosition()
        newFood = successorGameState.getFood()
        newGhostStates = successorGameState.getGhostStates()
        newScaredTimes = [ghostState.scaredTimer for ghostState in newGhostStates]
        newGhostPos = currentGameState.getGhostPositions()
        "*** YOUR CODE HERE ***"
        """Para la evaluación, nos basaremos en las distancias a la comida y a los fantasmas.
        Cuanto más cerca esté la comida, mejor puntuación, y cuanto más cerca esté el fantasma más cercano
        peor."""

        listaComida = newFood.asList()

        #Buscamos el fantasma más cercano
        fantasCerca = -min([manhattanDistance(newPos, ghost.getPosition()) for ghost in newGhostStates])

        #Trataremos las distancias en negativo -> Se buscan las mínimas pero mayor es mejor
        if fantasCerca:
            fantasCerca = 10/fantasCerca #Reducimos el valor de esta distancia ya que tener comida más cerca tiene más peso
        else: 
            fantasCerca= float('-inf') #Escogemos este valor si no hay fantasmas para que el no haber fantasmas no interfiera en el resultado

        if listaComida:
            comidaCerca = -3*min([manhattanDistance(newPos, comida) for comida in listaComida]) #No reducimos el valor de la comida ya que tiene más peso, se lo aumentamos
        else:
            comidaCerca = 0 #Si no hay comida hacemos que el único valor que se tenga en cuenta sea el de los fantasmas

        """Definimos el peso como la comida restante; le ifnluyen los valores calculados.
        El peso se multiplica por un número alto para que los valores calculados no influyan tanto el valor final"""
        valorTotal = comidaCerca + fantasCerca - (100*len(listaComida))
        #print(valorTotal)
        return valorTotal


def scoreEvaluationFunction(currentGameState):
    """
    This default evaluation function just returns the score of the state.
    The score is the same one displayed in the Pacman GUI.

    This evaluation function is meant for use with adversarial search agents
    (not reflex agents).
    """
    return currentGameState.getScore()

class MultiAgentSearchAgent(Agent):
    """
    This class provides some common elements to all of your
    multi-agent searchers.  Any methods defined here will be available
    to the MinimaxPacmanAgent, AlphaBetaPacmanAgent & ExpectimaxPacmanAgent.

    You *do not* need to make any changes here, but you can if you want to
    add functionality to all your adversarial search agents.  Please do not
    remove anything, however.

    Note: this is an abstract class: one that should not be instantiated.  It's
    only partially specified, and designed to be extended.  Agent (game.py)
    is another abstract class.
    """

    def __init__(self, evalFn = 'scoreEvaluationFunction', depth = '2'):
        self.index = 0 # Pacman is always agent index 0
        self.evaluationFunction = util.lookup(evalFn, globals())
        self.depth = int(depth)

class MinimaxAgent(MultiAgentSearchAgent):
    """
    Your minimax agent (question 2)
    """
    def getAction(self, gameState):
        """
        Returns the minimax action from the current gameState using self.depth
        and self.evaluationFunction.

        Here are some method calls that might be useful when implementing minimax.

        gameState.getLegalActions(agentIndex):
        Returns a list of legal actions for an agent
        agentIndex=0 means Pacman, ghosts are >= 1

        gameState.generateSuccessor(agentIndex, action):
        Returns the successor game state after an agent takes an action

        gameState.getNumAgents():
        Returns the total number of agents in the game

        gameState.isWin():
        Returns whether or not the game state is a winning state

        gameState.isLose():
        Returns whether or not the game state is a losing state
        """
        "*** YOUR CODE HERE ***"
        #print(self.depth)
        "Utilizamos tuplas para poder almacenar tanto el valor como la accion (lo que se devuelve al final)"
        def max_value(gameState,depth):
            #print(depth,gameState.getNumAgents(),self.depth*gameState.getNumAgents())
            """
            Si estamos en un estado final, 
                devolvemos la funcion de evaluacion en ese estado.
            Si no,
                Sacamos el maximo de entre sus sucesores, llamando a la funcion min 
                (ya que es sucesor sera un min)
            """
            v = float("-inf"), None

            if depth == self.depth*gameState.getNumAgents() or gameState.isWin() or gameState.isLose():
                return self.evaluationFunction(gameState),None
            
            for accion in gameState.getLegalActions(0):
                sucesor = gameState.generateSuccessor(0,accion)
                minvalue = min_value(sucesor,depth +1)

                if (v[0] < minvalue[0]):
                    v  = minvalue[0],accion
            return v

        def min_value(gameState,depth):
            """
            Si estamos en un estado final, 
                devolvemos la funcion de evaluacion en ese estado.
            Si no,
                Sacamos el minimo de entre sus sucesores, llamando a la funcion max o min 
                dependiendo del nivel en el que nos encontremos
            """
            v = float("inf"), None
            nivel_agente = depth%gameState.getNumAgents()

            if depth == self.depth*gameState.getNumAgents() or gameState.isWin() or gameState.isLose():
                return self.evaluationFunction(gameState),None

            for accion in gameState.getLegalActions(nivel_agente):
                sucesor = gameState.generateSuccessor(nivel_agente,accion)

                if nivel_agente == gameState.getNumAgents() -1:
                    valor = max_value(sucesor,depth + 1)
                else:
                    valor = min_value(sucesor,depth + 1)
                if (v[0] > valor[0]):
                    v  = valor[0],accion

            return v


        return max_value(gameState,0)[1]
        #util.raiseNotDefined()

class AlphaBetaAgent(MultiAgentSearchAgent):
    """
    Your minimax agent with alpha-beta pruning (question 3)
    """

    def getAction(self, gameState):
        """
        Returns the minimax action using self.depth and self.evaluationFunction
        """
        util.raiseNotDefined()

class ExpectimaxAgent(MultiAgentSearchAgent):
    """
      Your expectimax agent (question 4)
    """

    def getAction(self, gameState):
        """
        Returns the expectimax action using self.depth and self.evaluationFunction

        All ghosts should be modeled as choosing uniformly at random from their
        legal moves.
        """
        "*** YOUR CODE HERE ***"
        util.raiseNotDefined()

def betterEvaluationFunction(currentGameState):
    """
    Your extreme ghost-hunting, pellet-nabbing, food-gobbling, unstoppable
    evaluation function (question 5).

    DESCRIPTION: <write something here so we know what you did>
    """
    "*** YOUR CODE HERE ***"
    util.raiseNotDefined()

# Abbreviation
better = betterEvaluationFunction
