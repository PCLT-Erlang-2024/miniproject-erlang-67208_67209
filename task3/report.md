## Task 3

### Non-instant Trucks

In this variation of the system, it was asked that the trucks were no longer replaced instantaneously.
To achieve this, we added a timeout of ```TRUCK_REPLACEMENT_TIME``` on the truck process after stopping the belt process. This leads the conveyor belt to stop for a while before the truck is replaced.

For cosmetic reasons we have also added a timeout in between the generation and addition of packages to the conveyor belts.
