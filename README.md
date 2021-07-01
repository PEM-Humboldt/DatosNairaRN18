# DatosNairaRN18

Data processing of the outputs from the software Naira for the pictures taken by camera traps as part of the project with Riqueza Natural (RN) at 2018. 

This folder contains a script for this processing in R called "scpHexagonos.R". 

The first part of the script takes de different folders created by sampling hexagons and standardizes the structure of each file in order to join all registers. Then it creates a guide list of the start and end date of each hexagon in order to generate groups of replicates to build an occupancy matrix. 

Finally, the code takes the complete dataset and restructures the data in an DarwinCore output.

## Prerequisites

The R script calls a folder in "/data" that contains folders by sampling hexagon. 

These folders have excel files with the information from Naira for each register.

It also calls a csv file "list_sp_rv2" with two columns: "Species" and "Species_c", where "Species" is the list of species identified in the pictures and "Species_c" is the verified list of these species names.

Finally, for the creation of the DarwinCore output a csv file called "TablaEspecies_filled.csv" generated in "/Salidas" and then manually filled, has the information about kingdom, phylum and taxonRank by species identified.

## Authors and contact

* María Isabel Arce Plata - _Initial work_ - [MaIsAp](https://github.com/MaIsAp)
* Bibiana Gómez - _Data owner_ -