# Projet Google code Jam

#### Dorian Gouzou, Max Jean, Valentin Urcun  

_______________________________________________________
_______________________________________________________


### Description

Le projet se compose d'une série d'exercices, répartis en plusieurs rounds.  
Chaque exercice est indépendant.  
Voici la liste des exercices : 

* Qualifications
    * I/O error
    * Cody's Jam
    * Captain Hammer
* Round 1
    * Minimum Scalar Product
    * Triangle Trilemma
    * Make it Smooth
* Round 2
    * Rope Intranet
    * Center of Mass
* Round 3
    * Hexagon Game
    * Mountain View
    * Test Passing Probability
    * Evaluation
* Finale
    * Program within a Program
    * Travel Plan

### Utilisation
Vous devez d'abord vous déplacer dans le dossier où se situe le programme que vous voulez utiliser.  
Vous devrez ensuite utiliser les commandes suivantes :
```
rlwrap ocaml
# use "Nom_du_fichier.ml";;
jam "Fichier_d_Entree.in" "Fichier_de_Sortie" algolec fonction;;
```
Fichier_d_Entree.in se télécharge sur le site du concours Google Code Jam.  
L'algolec et la fonction à utiliser sont plus bas, ainsi qu'en fin de programme en commentaire 
Un Fichier_de_Sortie sera créé après exécution de la fonction (ou modifié s'il existe déjà). Il faudra le soumettre à validation, encore une fois via le site du concours Google Code Jam.



### Les Programmes

#### Qualifications
##### I/O error
La version small dure moins d'une seconde.
```
jam "A-small-practice.in" "output.out" algolec translate;;
```

##### Cody's Jam
La version small dure quelques secondes
```
jam "A-small-practice.in" "A-small.out" algolec algopb;;
```
La version large dure quelques secondes
```
jam "A-large-practice.in" "A-large.out" algolec algopb;;
```

##### Captain Hammer
La version small dure quelques secondes
```
jam "B-small-practice.in" "B-small.out" input_line callcaptain;;
```

#### Round 1
##### Minimum Scalar Product
La version small dure moins d'une seconde.
```
jam "A-small-practice.in" "output.out" algolec resolve;;
```
La version large dure quelques secondes.
```
jam "A-large-practice.in" "output.out" algolec resolve;;
```

##### Triangle Trilemma
La version small dure moins d'une seconde.
```
jam "A-small-practice.in" "output.out" input_line resolve;;
```
La version large dure moins d'une seconde.
```
jam "A-large-practice.in" "output.out" input_line resolve;;
```

##### Make it Smooth
La version small dure quelques secondes.
```
jam "B-small-practice.in" "B-small.out" algolec algopb;;
```
#### Round 2
##### Rope Intranet
La version small dure moins d'une seconde.
```
jam "A-small-practice.in" "output.out" algolec resolve;;
```
La version large dure moins d'une seconde.
```
jam "A-large-practice.in" "output.out" algolec resolve;;
```

##### Center of Mass
La version small dure quelques secondes.
```
jam "C-small-practice.in" "C-small.out" algolec fireflies;;
```
La version large dure quelques secondes.
```
jam "C-large-practice.in" "C-large.out" algolec fireflies;;
```


#### Round 3
##### Hexagon Game
La version small dure moins d'une seconde.
```
jam "D-small-practice.in" "output.out" algolec resolve;;
```
La version large dure de 3 min environ à 9 min, selon l'ordinateur utilisé.
```
jam "D-large-practice.in" "output.out" algolec resolve;;
```

##### Mountain View
La version small dure quelques secondes.
```
jam "C-small-practice.in" "C-small.out" algolec algopb;;
```
##### Test Passing Probability
La version small dure quelques secondes.
```
jam "C-small-practice.in" "C-small.out" algolec algopb;;
```
##### Evaluation
La version small dure quelques secondes.
```
jam "C-small-practice.in" "C-small.out" algolec algopb;;
```
La version large dure quelques minutes.
```
jam "C-large-practice.in" "C-large.out" algolec algopb;;
```


#### Finale
##### Program within a Program
La version small dure quelques secondes.
```
jam "C-small-practice.in" "C-small.out" input_line program;;
```
La version large dure quelques secondes.
```
jam "C-large-practice.in" "C-large.out" input_line program;;
```
##### Travel Plan
La version small dure quelques secondes.
Il y a 2 programmes : 
d.ml ne règle que le small
d3N2.ml règle le small et théoriquement le large
```
jam "D-small-practice.in" "D-small.out" algolec travel;;
```
La version large est estimée à quelques semaines.
```
# use "d3N2.ml"
jam "D-large-practice.in" "D-large.out" algolec travel;;
```



