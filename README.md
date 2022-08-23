# MusicRec 
MusicRec is a content-based recommender system that, exploiting Prolog inference, is capable of providing advices about songs and musical artists based on insights about mood and genres preferences expressed by the user. 
In particular, the knowledge of the domain that the system uses for reasoning is provided with a knowledge graph, which stores everything about tracks, their features, albums, artists and musical genres.
The application can be accessed by the user through a dialogue with a graphical interface: the system will ask the user some questions and, after that, a first bunch of suggestions will be delivered to the user, who is capable of refining it, providing further information and explicit approval.

## Requirements
The system doesn't need any particular prerequisite in terms of computational effort or requirements to be satisfied before to run the application. 
It just needs Python and SWI Prolog to be installed on the machine in order to perform inference, and the files related to the dataset that contain the expertness of the domain to be downloaded in the same folder of the project (these are already present when downloading the repository). 

## Installation
1. Clone the repository directly from GitHub or download the zip archive and extract it. 
2. Open the folder and click on install.bat: this batch file will installed the required dependencies to correctly run the project, including Streamlit and PySwip. 
3. When finishing the procedure, an automatic browser tab will open with the user interface and the system will be ready to be used.

## Overview on components
The system is composed of different parts that guarantee the correctness of the workflow, from the loading of the knowledge base until the production of the final output. 
The needed components are:
- Knowledge graph containing the expertness of the domain
- Prolog rules and inference modules
- Interface dialogue module
- Connection module between backend and frontend
