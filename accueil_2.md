---
title: "DYPOP"
author: "Victor Bret & Fabrice Zaoui"
output: html_document
---

Le __modèle hiérarchique__ mis en &#339;uvre vise à estimer les processus communs de cette dynamique de population (à l'échelle de la France), mais également à appréhender l'origine des variations observées au niveau de chaque cours d'eau étudié.

De plus le cadre __Bayésien__ dans lequel le modèle a été développé permet de tenir compte explicitement de l'incertitude (liée aux échantillonnages, aux paramètres estimés, aux prédictions).

Ces travaux se sont appuyés sur 428 échantillons piscicoles, réalisés sur 40 tronçons de cours d'eau français dont 19 en tronçon court-circuité à l'aval d'un barrage hydro-électrique. Ces données piscicoles donnaient une vision annuelle de l'état des populations à travers leurs densités (exprimées en __nombre d'individu par 100 m^2__). Elles étaient associées à des données de conditions du milieu (physique hydraulique et thermique).

__ATTENTION : il n'est pas pertinent d'extrapoler les résultats de ces analyses hors des contextes étudiés (autre espèce piscicole, régime thermique ou hydraulique très différent, etc.)__

_Plus d'informations sur le jeu de données utilisé pour construire le modèle sont disponibles sous l'onglet "A propos"._

Le modèle a permis d'expliquer les fortes variations de survies entre tronçons étudiés par les conditions du milieu :

* la disponibilité d'abris favorise la survie en réduisant les mortalités densité-dépendantes des truites de 1 an (stade 1+);
* le régime thermique influence également la mortalité densité-dépendante : de faibles températures (T90) réduisent la survie des 1+ alors que de fortes températures (T10) sont associées à de plus faible survie des adultes (>1+).

Cette interface constitue un outil d'aide à la décision dans différentes situations opérationnelles, comme :

* L'évaluation de __l'état d'une population naturelle__, échantillonnée au moins une fois.
  _Les densités et/ou taux de survie observés sont-ils en phase avec les prédictions ou observe-t-on des valeurs atypiques pour ce type de contexte (physique et thermique)?_
* La mise en &#339;uvre de __projets de restauration de l'habitat physique__.
  _Quel serait l'effet attendu de l'ajout de surface de caches sur la survie des individus?_
* Des projets de __rempoissonnement__.
  _Les caractéristiques physiques du cours d'eau permettent-elles un rempoissonnement efficace ou apparaissent-elles comme limitantes (prédiction de fortes mortalités densité-dépendantes)?_
* L'évalutation de l'influence d'un __changement de régime thermique__ sur la dynamique de population en place.
  _Quel serait l'effet attendu d'une augmentation de la température de l'eau sur la survie des individus?_
