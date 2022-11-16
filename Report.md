# Report on the Net-list Simulator  

## Behavior :  

We start by running a topological sort on the netlist, where for all instructions a and b such that a depends on b, b appears before a. We also assure registers appear first.  
We then print the sorted netlist in a new file, and run a "dummy cycle" (where every variable is set to 0), to try and catch typing errors.
Then, if we are not in print-only mode, we run the simulator for the number of steps required (or until the user exits if unspecified). 

The README file explains how to run the simulator, and provides explanations and examples on possible inputs (the ROM and the variables) and errors.  
The following will expand on how we handled certains points which we feel may need further explanation.  

### Storing the variables :  

We store each variable in a hashmap (values), which each one initialized at 0. Note that single bits and buses containing a single bit are functionally identital : as such, to reduce the cases that must be considered, all variables are considered as buses.  

### Registers :  

To handle registers, we give them higher priority than any other function (as in, they will be executed first in any cycle), and reg(x) will read the value stored for x in our hashmap (which will be the value of the previous cycle, since we haven't modified it yet [That property may be false if we have a = reg(b) and b = reg(a). However, in that case, we only modify these values when doing the register operation, and so they will stay permanently as initialized, which is to say 0.]).  

### The ROM / RAM :  

Both the ROM and RAM are hashmaps, having boolean arrays as both keys and values. The RAM will be initialized as empty. The ROM will, depending on whether the user inputs another file or not, be initialized to whatever that program dictates (see the README for the syntax).  
When accessing either of them, the adress provided may be a key which is unassigned in the hashmap. In that case, we assume the cell is full of zeroes.  

To fully handle the RAM, we proceed in two steps : First read the RAM during the cycle as usual, ignoring writing instructions. Then, after the cycle, re-run through every instruction, searching for writing steps then.  

### Error Handling :  

We try to handle any error native to the provided to the net-list in all use cases (combinatory cycles, syntax errors or wrong typing). We also raise errors when the provided ROM or the input is wrong : in the last case, the user may input another value and correct that error.
