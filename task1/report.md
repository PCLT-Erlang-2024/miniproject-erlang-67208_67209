## Task 1 & Project Details

### Objective

The objective of this assignment was to implement the core product reservation system. The implementation must account for multiple conveyor belts being "fed" packages and multiple trucks, assuming that when a truck is full or the arriving package is too big it can be replaced by a new truck instantaneously, meaning that the conveyor belts can run continuously.

### Implementation

To explain the logic flow of the design of our implementation and how each component interacts with each other we have created the
following illustration:

![Project flow](https://github.com/cpldi/go-mini1-67208_67209/blob/main/Illustration.png)

The ```receive``` statement is crucial for the actor based concurrency model to allow processes to comunicate with each other.

### Components
The following are the key components of the project. While all tasks were completed collaboratively, we have outlined who was the main
responsible for each component of the project.

- Truck struct class; (Tomás Carvalho)
- Package struct class; (Tomás Carvalho)
- Logging; (Francisco Barreiras)
- Package Generator; (Francisco Barreiras)
- Belt Process; (Francisco Barreiras)
- Truck Generator; (Tomás Carvalho)
- Truck Loading; (Ambos)
- Task 2; (Tomás Carvalho)
- Task 3; (Francisco Barreiras)
