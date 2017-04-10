# NetCalBounds
Network Calclus performance bounds

This repository contains some programs to compute worst-case delay bounds in tandem networks ion the framework of Network calulus. 
They rely on the results of the following publications: 

**For FIFO networks:**

[1] Anne Bouillard and Giovanni Stea, *Exact Worst-case Delay in FIFO-multiplexing
Feed-forward Networks*, IEEE/ACM Transactions on Networking, 2015

[2] Anne Bouillard and Giovanni Stea, *Exact Worst-case Delay for FIFO-multiplexing tandems*, Sixth International Conference on Performance Evaluation Methodologies and Tools (ValueTools 2012)

**For Arbitrary multiplexing networks: (to come)**

[3] Anne Bouillard and Eric Thierry, *Tight performance bounds in the worst-case analysis
    of feed-forward networks*, Journal of Discrete Event Dynamic Systems, 2016
    
[4] Anne Bouillard, Laurent Jouhet and Eric Thierry, *Tight Performance Bounds in the Worst-Case Analysis of Feed-Forward Networks*, 29th IEEE International Conference on Computer Communications (INFOCOM 2010)

Programs are written in OCaml, and comments are in French. They are all based on the same principle: a program is used to generate a linear program (lp_solve format). 

## FIFO networks

- **fifoWCD.ml** computes the worst-case delay in a FIFO tandem networks (returns a mixed-integer linear program with an exponential number of constraints in the size of the network)
- **fifoLB.ml** computes a lower bound of the worst-case delay (returns a linear program with a polynomial number of constraints)
- **fifoUB.ml** computes an upper bound of the worst-case delay (returns a linear program with an exponential number of constraints)

### Usage

- **Compilation**(with native compilers): ```ocamlopt -pp camlp4o fifoWCD.ml -o fifoWCD```
- **Execution**: ```./fifoWCD network.txt network.lp```
- **Bound computation**: ```lp_solve network.lp```

The useage is exactly the same for the two other files.

### Input file format

The program takes an input file (network.txt above), and outputs a lp_solve file (network.lp above). 
The input format follows the example given below: 

```
nservers 6        #number of servers
nflows 7          # number of flows
objective 'd' 1   #'d' i = maximal delay for flow i; 'b' j = maximal backlog at server j

#list of server's characteristics (servers numbered from left to right)
beginservers 
server 1 (10.0,1.0);    #(rate,latency) list. The service curve is the maximum.
server 2 (10.0,1.0);
server 3 (10.0,1.0);
server 4 (10.0,1.0);
server 5 (10.0,1.0);
server 6 (10.0,1.0);
endservers

#list of flows 's characteristics (servers numbered from left to right)
beginflows 
flow 1 [1,6]	(0.,10.) (1.,5.); #[source,destination] (burst,rate) list the arrival curve in the minimum
flow 2 [1,1]   	(0.,10.) (1.,5.);
flow 3 [2,2]  	(0.,10.) (1.,5.);
flow 4 [3,3] 	(0.,10.) (1.,5.);
flow 5 [4,4] 	(0.,10.) (1.,5.);
flow 6 [5,5] 	(0.,10.) (1.,5.);
flow 6 [6,6] 	(0.,10.) (1.,5.);
endflows
```



