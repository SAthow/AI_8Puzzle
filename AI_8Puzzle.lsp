#|
        ***** AI_8Puzzle.lsp *****

This program

Author: John M. Weiss, Ph.D.
Posted Spring 2016 for SDSM&T CSC447/547 Artificial Intelligence.
|#
(defvar *start* '())
(defvar *goal* '())
(defvar *open* '())
(defvar *close* '())
(defvar *dimension* 0)
(devar *moveCount* 0)
(devar *generatedCount* 0)
(devar *expandedCount* 0)
(devar *distinctNodes* 0)
(devar *duplicateNodes* 0)