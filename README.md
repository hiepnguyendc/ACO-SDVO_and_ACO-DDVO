# ACO-DDVO
ACO-DDVO is a tool coded by FORTRAN to support for optimization of irrigation scheduling. It represents the irrigation scheduling problem in the form of a decision tree and use ant colony optimization (ACO) as the optimization engine. It can dynamically adjust decision variable options during the optimization process. Therefore, ACO-DDVO is able to reduce search space size and increase the computational efficiency of ACO.algorithm.



ACO-DDVO considers two formulations:

- Formulation 1: There is only one decision variable, i.e. crop type. The softwate for Formulation 1 is stored in the branch "Formulation-1".

- Formulation 2: There are two decision variables, including crop type and the depth of irrigation water. The softwate for Formulation 2 is stored in the branch "Formulation-2"
 
The input data for both formulations are stored in the branch "Input_Data".
