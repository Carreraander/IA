# -*- coding: utf-8 -*-
# mira.py
# -------
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


# Mira implementation
import util
PRINT = True



class MiraClassifier:
    """
    Mira classifier.

    Note that the variable 'datum' in this code refers to a counter of features
    (not to a raw samples.Datum).
    """
    def __init__( self, legalLabels, max_iterations):
        self.legalLabels = legalLabels
        self.type = "mira"
        self.automaticTuning = False
        self.C = 0.001
        self.legalLabels = legalLabels
        self.max_iterations = max_iterations
        self.initializeWeightsToZero()

    def initializeWeightsToZero(self):
        "Resets the weights of each label to zero vectors"
        self.weights = {}
        for label in self.legalLabels:
            self.weights[label] = util.Counter() # this is the data-structure you should use

    def train(self, trainingData, trainingLabels, validationData, validationLabels):
        "Outside shell to call your method. Do not modify this method."

        self.features = trainingData[0].keys() # this could be useful for your code later...

        if (self.automaticTuning):
            Cgrid = [0.002, 0.004, 0.008]
        else:
            Cgrid = [self.C]

        return self.trainAndTune(trainingData, trainingLabels, validationData, validationLabels, Cgrid)

    def trainAndTune(self, trainingData, trainingLabels, validationData, validationLabels, Cgrid):
        """
        This method sets self.weights using MIRA.  Train the classifier for each value of C in Cgrid,
        then store the weights that give the best accuracy on the validationData.

        Use the provided self.weights[label] data structure so that
        the classify method works correctly. Also, recall that a
        datum is a counter from features to values for those features
        representing a vector of values.
        """
        "*** YOUR CODE HERE ***"
        """
        elegir una actualizacion que arregle la
        erronea clasificación del ejemplo actual de
        entrenamiento minimizando el cambio sobre W.
            min(1/2*sum(w-wy)^2)
            𝑤𝑦∗⋅𝑓𝑥≥𝑤𝑦⋅𝑓𝑥+𝟏
            𝜏∗=𝑚𝑖𝑛(𝑤′𝑦−𝑤′𝑦∗⋅𝑓+1)/(2𝑓⋅𝑓),𝐶)
        """
        """
        Variables:
            Cgrid[]: Grid de las constantes C.
            max_iterations: Numero maximo de iteraciones para no sobreajustar.
            weights[]: Pesos para cada label.
            tau: Variable para no realizar actualiaciones demasiado grandes.
            trainingData, trainingLabels, validationLabels, validationData: labels y datos de train/test.
            accuracy: Tasa de acierto(?)
            prediccion: utilizar la funcion classify()
            ...
            
        """

        for c in Cgrid:
            for iteration in range(self.max_iterations):
                for i in range(len(trainingData)):
                    #No me vale un 0 como labelpre obviamente
                    labelpre = -1
                    toplabel = None
                    for label in self.legalLabels:
                        prediccion = self.weights[label] * trainingData[i]
                        if labelpre < prediccion:
                            #Guardamos los resultados con mayor probabilidad de acierto
                            labelpre = prediccion
                            toplabel = label

                    if toplabel != trainingLabels[i]: #No esta bien predicho
                        #𝜏=𝑚𝑖𝑛(𝑤′𝑦−𝑤′𝑦∗⋅𝑓+1)/(2𝑓⋅𝑓),𝐶)
                        tau = min(c,(((self.weights[toplabel] - self.weights[trainingLabels[i]]) * trainingData[i] + 1.0) / ( 2 * (trainingData[i] * trainingData[i]))))
                        
                        #Unica manera de poder multiplicar f por tau (Ya que f esta definido como un counter)
                        tau2 = trainingData[i].copy()
                        tau2.divideAll(1.0/tau)
                        #Ajustes
                        self.weights[toplabel] -= tau2
                        self.weights[trainingLabels[i]] += tau2


        #util.raiseNotDefined()

    def classify(self, data ):
        """
        Classifies each datum as the label that most closely matches the prototype vector
        for that label.  See the project description for details.

        Recall that a datum is a util.counter...
        """
        guesses = []
        for datum in data:
            vectors = util.Counter()
            for l in self.legalLabels:
                vectors[l] = self.weights[l] * datum
            guesses.append(vectors.argMax())
        return guesses


