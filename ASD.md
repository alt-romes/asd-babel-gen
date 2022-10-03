## Fault Model

Fault -> Error -> Failure

Fault is unavoidable, Failure is. We should not allow failure, but tolerate faults.

##  Process Fault Model

* Correct process
* Faulty processes:
  * Crash Fault Model: Process fails and stops sending any messages (good)
  * Omission Fault Model: Process  that fails/omits the transmission of any number of messages (potentially not all of them)
  * Fail-Stop Model: Like Crash Fault Model, but when a process crashes it notifies all other processes of its own failure
  * Byzantine (or Arbitrary) Fault Model: Failed process will do whatever and not respect the protocol

## Network Model

Model network as independent *links* that interconnect processes. It captures what is going wrong in the network.
Models here have properties (usually numbered)

* Fair loss model
* Stubborn model
* Perfect link model


Algorithms Specification and Properties
===

Two fundamental typs of properties are *safety* and *liveliness*

A safety property is a condition that must be enforced at any (and all) times
(bad things should never happen), e.g. No Duplication. ??

Liveness properties says that something useful will happen. Conditions that
should be enforced at some points of the execution (good things that should
happen eventually), e.g. Eventual Delivery.

Safety vs Liveness:
Correct algorithms will have both safety and liveness properties.
Some mixed properties can be decomposed into a conjunction of simpler safety and liveness properties.

Compose Protocols
===

How?


Liveness properties makes it so that something useful happens.
With only safety properties we could do nothing and be safe.

---

The broadcast problem:

Come up with some protocol: a process can send a message to every other process including himself
Assume:
  Set of processes are known a-priori: π
  Assume perfect point-to-point link abstraction (eventual delivery, no duplication, no creation).

Reliable Broadcast:
* RB1 (Validity)
* RB2 (No Duplication)
* RB3 (No Creation)
* RB4 (Agreement)

---

Homework 1:

Write the pseudo code for solving the reliable broadcast problem assuming:
* A fail stop model and a synchronous system.
* Your solution must ensure all properties of the reliable broadest problem
* Your solution should ensure that in fault free executions each process
collaborates in the dissemination by sending a single message

Hints:
* Since we are in synchronous environment and in the fail stop model, when a
process crashes all processes will trigger an event: Upon crash(p)
* You can assume that process identifiers are sequential numbers starting at 1
and going up to #(π).
* The special init event can receive arguments, which might be useful for you
to receive both π the local process identifier p

---

Defining algorithms:

The model of a process is a deterministic state automaton.

Starts with state (that each process has)

---
---

## Uniform Reliable Broadcast

-- impossible to solve without extra restrictions


In general, to solve these problems  you must assume a maximum  number of
processes that can crash (f \le N). In this case, before you deliver a message,
more than f processes must have a copy of the message, because, to make sure
that at least one process will survive that has a copy of the message that there
is a way for all other correct processes to eventually deliver it.

### Review on broadcast solutions

If we have a process that wants to broadcast, the process sends a message to
itself and then to the others and then everyone delivers. If someone crashes we
rebroadcast it. This is problematic because all the load of broadcasting is on
one single node (especially if the number of nodes keeps growing).

## Gossip Protocols

How does it work? Pick some nodes at random and send them the message. Then they
pick at random other nodes and send again. Eventually everyone will receive the
message. We have some  redundant messages, but everyone got the broadcasted
message, so we are happy. This is gossip :-), because the algorithm mimics the
way a gossip spreads across population.  It's also known as an epidemic
algorithm.


## A simple gossip algorithm/protocol

A process  𝑝 wants to broadcast 𝑚. It picks 𝑡 other processes from the  system
uniformly at random (to preserve probabilistic guarantees),  and sends them 𝑚.t

When a process receives a message for the first  time, it simply repeats this
process (eventually avoiding to send the message back to the sender)

How to choose 𝑡 (gossip targets)? The theory of epidemics says that  to ensure
high probability (tends to 1) that everyone receives the message, 𝑡>= ln(# π). This (𝑡) is
usually named the *fanout* of the algorithm.

Note that this isn't a "reliable" protocol since it is only very probable but
not sure that everyone receives the message.

A low amount of 𝑝 processes (\le 3) is "more dangerous", but it scales up really
well.

So we introduce:

## Probabilistic Reliable Broadcast (AKA Epidemic Broadcast)

* PRB1 (Validity)
* PRB2 (No Duplication)
* PRB3 (No Creation)
* PRB4 (Agreement) (based on probability)

## Gossip Redundancy

* We have a lot of redundancy, which is good since we are operating on top of fair
loss links, even though we do waste e.g. more network resources. Also, the
redundancy is what makes this correct despite fails.
* On average, each process will receive the message 𝑡 times.
* Total cost of messages: #π*𝑡

#### What if the message is really big?

In this case, it's a bad idea, so we can use something like **pull gossip**
(as opposed to (eager) push gossip)

### Gossip Types

[Sender] ---- message ---> [Receiver]  :: Push Gossip

[Sender] <--- do you have  [Receiver]  :: vvv
[Sender] ---- message ---> [Receiver]  :: Pull Gossip

[Sender] ---- mid -------> [Receiver]  :: vvv
[Sender] <- send me m with mid [Receiver]  :: vvv
[Sender] ---- message ---> [Receiver]  :: Lazy Push Gossip


Push Gossip:
* Fast
* If messages are big -> expensive

Pull Gossip:
* If messages are big -> less network traffic
* Slow
* If messages are small more traffic


Lazy Push Gossip:
* Faster than p ull
* More communication steps
* If messages are small will have too much? network traffic

Combined
* e.g., at the beginning do push and then switch to lazy, or so...


Related problems: Topology Mismatch, but will be a bad bias on the supposedly
uniform sampling.


The gossip protocol assumes a **global known membership**. In
practice, someone who joins the network will have to notify all other
processes of its membership.

### Why avoid a global known membership
In a very large system π is not static, and keeping this information up to date
is the costly part.

### Partial view membership system

Each process in the system knows some other processes in the system. This will
generate a (virtual) network (an application level network) on top of the
physical network. We call this an **overlay network** (since it's operating on
top of another network).

## Overlay Network

* Nodes define (application level/logical) neighbouring relationships (materialized by links)
* Correctness:
    * Connectivity: There must be a path connecting any correct process p to
        every other correct process v.
    * Accuracy: Eventually, no correct process p will have an overlay link to a
        failed process (maximize accuracy between 0 and 1. If everyone crashed
        it's 0, if everyone  is alive it's 1)

*  Efficiency:
    * Low diameter: paths between correct processes should be small (measured by
        the average shortest path)
    * Low clustering: the neighbours of each process should be as different as
        possible from the neighbours of each of its neighbours
    * Uniform degree: All processes should have a similar  number of neighbours (degree) (in degree and out degree could be different)
        (If gossiping, the out degree should be at least the same as the fanout)

* We can build an unstructured overlay network through gossiping too.


<!--
### Use case: Cyclon

Keep a set of neighbours with a maximum size (defined as more than the ...).
Process identifiers are enriched with an age.
Picks the one with the highest age (we update the age of each process in our
view). If dead, update process list.
He destroys the identifier of the process its contacting and creates a new
identifier for himself. From the other side, they update their views according
to the sent list of neighbours.
Periodically remove one node and send him your new identifier with a sample from
your list of neighbours
-->

## Gossip Summary

* Degree of the overlay (number of neighbours: N)
* Fanout (t)
* Communication Mode:
    * Eager push
    * Pull
    * Lazy push

* Epidemic broadcast:
    * 
    * 


* Flood:
    * t = N
    * 

* Anti-entropy:
    * t = 1
    * Pull (executed by everyone periodically)

* Random-walk (useful to look for stuff in the overlay):
    * t = 1
    * Eager push (usually associated with a maximum number of jumps a message
        can do aka "time to live" or "TTL")
