---
title: "DYPOP"
author: "Victor Bret & Fabrice Zaoui"
output: html_document
---

Pour prédire les densités d’adultes (>1+) susceptibles d’être observées, il faut prédire la survie :
- des adultes observés l’année précédente qui seront toujours vivants au sein de la station ;
-  et des 1+ observés l’année précédente devenus adultes l’année considérée.

La prédiction des densités d’adultes en place est donc à représenter en trois dimensions, à l’aide d’une carte de chaleur (par défaut) ou en relief (en cochant la case « vue 3D »). Pour faciliter la lecture, seule la prédiction médiane est représentée sur le panneau central.

Les panneaux supérieurs et de droite représentent les relations marginales :
Panneau supérieur : évolution des densités d’adultes en fonction des densités d’adultes observées l’année précédente (la densité de 1+ observée l’année précédente est fixe, indiquée sur l’échelle en bas à droite - si possible ajouter un lien hypertexte vers le titre de l’échelle -).
Panneau de droite : évolution des densités d’adultes en fonction des densités de 1+ observées l’année précédente (la densité d’adultes observée l’année précédente est fixe, indiquée sur l’échelle en bas à gauche - si possible ajouter un lien hypertexte vers le titre de l’échelle -).

Ces visions marginales permettent de représenter l’enveloppe d’incertitude autour de ces prédictions :
- La ligne rouge correspond à la prédiction médiane.
- 50% des densités prédites par le modèle sont contenues dans l’enveloppe gris foncé.
- 95% des densités prédites par le modèle sont contenues dans l’enveloppe gris clair.

