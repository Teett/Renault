Hi There!

This repository contains some data science scripts used for 2 different projects during my 6 months at Renault - Colombia (SOFASA) using R:

Vehicle project risk assesment tool: Every part of a new car needs to be validated by a pool of experts in France before it can be implemented in a vehicle. Therefore, it is natural that some of those drawings get rejected. This project aims to assess:
1. What is the current state of validation of parts in the project (a car) in 3 components (The materials of the part (MDS), The drawing of the part (2D), the digital Mockup of the part (3D).
2. Which drawings are the most likely to be rejected and should be given more attention?

Furthermore, a classification algorithm was implemented (Elastic-net regression) using the glmnet package, that uses the validation comments / verbatims scrapped with UIpath from NPDM (The project management software from Renault) written by the validation experts. For more detail, consult the technical sheet of the model (In spanish at the moment):

![Technical Sheet](https://raw.githubusercontent.com/Teett/Renault/master/Vehicle%20project%20risk%20assesment%20tool/model%20technical%20sheet%20-%20spanish.png?token=ALCDT4FJO2MTPGJRGODTLLC6CNVMW)
