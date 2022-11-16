# Netlist-Simulator-2022-2023 :

This is a simulator taking an (unordered) net-list to print the ordered corresponding net-list and execute it. 

## Compiling the simulator :

Make sure to execute the following command to compile the simulator (before running it for the first time) :  
> ocamlbuild netlist_simulator.byte

## Running the simulator :

To run the simulator, execute the following command :
> ./netlist_simulator.byte [flags] [file]

### Flags :
- > -n [integer]  : The simulator will run for [integer] cycles. If the flag isn't specified, the simulator will run until user exit.
- > --print  : The simulator will only print the ordered netlist. This differs from using -n 0 as it will not ask the user how to initialize the ROM if needed.

### User Input : 
#### The ROM :

If the program uses the ROM, the program will first ask the user for the name of a file which contains the ROM. The file must be written as such :  
> [adress]:[data] [adress]:[data] ...  

Note that spaces may be replaced by line breaks.  

Both the adress and the data must be represented as a sequence of 0s and 1s, e.g. :

> 101110:101 010011:001 

If the input is incorrect, the program will return an error, and exit.  
Every uninitialized adress will be considered to contain as many 0s as needed while read. As such, if the user wishes to initialize the ROM as full of zeroes, they may input no file (instead of an empty one).

#### Variable input :

When prompted for a variable, the user must write a sequence of 0s and 1s of the correct length depending on the variable. If the input is incorrect, the user will be prompted to re-enter an input (the length of the sequence will be given in the error). E.g. :

> a ? 1011  
This should be 0 or 1.  
a ? 1  
b ? 10  
This should be a sequence of 0 or 1 of length 3.  
b ? 101  

## Possible errors :
The simulator will return errors (and exit) in the following cases :  
- The netlist is parsed incorrectly : Always.
- The netlist has a combinatory cycle : Always.  
- Incorrect variable use (e.g. selecting element 3 in a 2-length array, trying to read or write to an adress which hasn't got the right size...) : Always.
- The ROM has incorrect syntax : After being prompted for the ROM.  
- The input for a variable is incorrect : After being prompted for that input. Instead of exiting, the simulator will ask again.  





