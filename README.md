# DatosNairaRN18

Data processing of the outputs from the software Naira for the pictures taken by camera traps as part of the project with Riqueza Natural (RN) at 2018. 

This folder contains a script for this processing in R called `scpHexagonos.R`.

The first part of the script takes de different folders created by sampling hexagons and standardizes the structure of each file in order to join all registers. Then it creates a guide list of the start and end date of each hexagon in order to generate groups of replicates to build an occupancy matrix. 

Finally, the code takes the complete dataset and restructures the data in a DarwinCore output.

## Prerequisites

The R script calls a folder in `/data` that contains folders by sampling hexagon. 

These folders have excel files with the information from Naira for each register.

It also calls a csv file `list_sp_rv2` with two columns: `Species` and `Species_c`, where `Species` is the list of species identified in the pictures taken with the camera traps and `Species_c` is the verified list of these species names.

Finally, for the creation of the DarwinCore output a csv file called TablaEspecies_filled.csv` generated in `/Salidas` and then manually filled, has the information about kingdom, phylum and taxonRank by species identified and is used to complete the information needed to generate de DarwinCore file.

## How to run

1. Load the libraries that will be used. 

2. Consider that in the code vectors names will start with v , dataframes with d , lists with l and functions with fun .

3. Load the data from the folders in `/data` with the Naira outputs.

4. Standardize the data to create `dHexData`, a dataframe that joins all the files exported with Naira.

5. Then if needed, create dates groups to define replicates for the occupancy matrix.

6. Then organize the data to create a dataframe with the structure for DarwinCore.

## Authors and contact

* **María Isabel Arce Plata** - *Initial work* - [MaIsAp](https://github.com/MaIsAp)
* **Bibiana Gómez** - *Data owner* -

## License

This project is licensed under the MIT License.

## Acknowledgements

* To the local communities in Montes de Maria that worked as guides and even lend their homes to the researchers for their stay.
* To the researchers from the program of Biodiversity Monitoring and Assessment who took from their time to go to the field and mounted the cameras and then went to unmount them.