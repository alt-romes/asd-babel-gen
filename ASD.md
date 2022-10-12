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



-----


## Unstructured overlays based on Super-Peers

* Small fraction of processes (with more resources, more power) are promoted to
    Super-Peers
* Super-Peers form an unstructured overlay among them
* Regular process connect to a super-peer and transimt to it the index of its
    resources
* Queries are forwarded to the super-peer and then are disseminated only among
    super-peers
* Pros: singificantly erduces the number of messages
* Cons: How to decide who is the super peer? Load in the system is highly
    unbalanced. Fault model is no longer uniform too

## Resource location (exact match)

Given a set of processes containting different sets of resources, locate the
processes that contain a given  resource given its unique identifier

### Consistent Hashing

Very common technique in distributed systems

When we apply a hash function to the same input we get the same output...

With consistent hashing we can form a DHT to solve our problem ?

* Attribute a hash to  each process based on  unique determinants
* Use the same hash function  to generate hashes for  resources  to store in
    the network
* Process whose hash is closest to the resource's hash keeps the information
    regarding where the resource is  (full details)
    * First, the one registering the resource registers it in the  closest node
    * Second, nodes looking  for the resource query the node closest to the
        resource hash and ask it for the location of the resource (which might
        have to periodically updated by the node that registered it)

When we have the full membership, this allows to build a **One-Hop Distributed
Hash  Table (DHT)**,  which is quite popular in ...

But we can do better and without assuming full membership which is quite costly
on big enough  networks

## Structured Overlay Networks

Basically, it has a fixed topology, and we can take advantage of that to make
sure messages go in the right direction. We know the topology a-priori :)


First step:

The  most common structured overlay topology is a ring, that connects nodes in
order considering their identifiers

In fact, the ring structure, in this type of structured overlay is the prime
correctness criteria

This is not good enough because of **long paths between processes**. It'd be
particularly bad for a node to crash since it breaks the ring.

Second step:

We add some additional overlay links (in the order of ln (#π)) to speedup
things.

(1) We now have information to deal with faults,
(2) and we can reach any other process in a worst-case logarithmic number of hops.

The non-trivial part is how to build this structured overlay network.

Relevant Examples (Big literature):

* Chord
* Pastry
* Kademlia



## The Chord Protocol

Assuming  process identifiers have m bits

State:
    // finger[k] | first node on circle that succeeds (n + 2^(k-1)  mod 2^m,  1 <= k <= m           // >>
    // successor | the next node on the identifier circle;  finger[1].node
    // predecessor | the previous node on the identifier circle



    // We might have multiple positions in the finger table occupied by the same process


Key functionality of Chord is to answer "Who is the  node  responsible for this
identifier?":

// ask  node n to find the successor of id
n.find.successor(id):

// search the local  table for the  highest predecessor of id
n.closest.preceding_node(id)

The rest is to keep the two above methods possible

// create a new Chord ring
n.create() // as Upon Init()
    predecessor = nil
    sucessor = n // himself, on beginning its  just a ring of you

n.join(n'):
    predecessor = nil
    sucessor = n'.find_sucessor(n)


Ensure that  the ring converges; stabilizing  the ring:

// called periodically,  verifies n's immediate
// must be stabilized often enough or ring might accumulate  faults  and crumble
n.stabilize()
    x = sucessor.predecessor;
    if (x \in (n, sucessor)) // if is correct then it should be me; otherwise something has happened
        sucessor = x  // either way we accept it is such
    sucessor.notify(n)

// n' thinks it might be our predecessor
n.notify(n'):
    // so we upate him  if he should indeed be our predecessor
    // which is when it is nil (has failed or just joined)
    // or when it is actually in between the current predecessor and us
    if (predecessor is nil or n' \in (predecessor, n))
        predecessor = n'

// called periodically, refreshses finger table entries
// next stores the index of the next finger to fix
n.fix_fingers()
    next = next +  1
    if (next > m)
        next  = 1;
    finger[next]  = find_sucessor(n+2^(next-1))

//called periodically, checks  whether predecessor has failed
n.check_predecessor()
    if  (predecessor has failed) // send a message  to predecessor in order  to confirm your reply. if there's no reply we assume it failed
        predeessor = nil

The successor might not always be correct. Anything that happens in the section
in which the  successor is incorrect might lead to incorrect nodes

### Other Structured Overlay Networks

Other type of organization of nodes  that isn't necessarily a ring but also has
an a-priori topology. The trick is that the more restrictions we add to the
topology, the more difficult the implementation and corner cases

e.g. a Torus is more fault tolerant and a hyper cube is faster :)

Other examples:

Tree-based overlay networks:
* Good for disseminate messages with low overhead
* Also good to aggregate information
* not failulre resistant

## Overview of Overlays

* Unstructured (or Random)

    * Even if someone fails, we can easily replace i t
    * Easy to build and maintain
    * Limited efficiency for some use cases

* Structured
    *  Very efficient for particular types of applications  since  we can take
        advantage of our knowledge of the topology
    * Less robust to failures (a failed process can only be replaed by a
        limited numbr of processes)
    * More complex and costly algorithms



------

## Kademlia

* Identifier space is seen as a binary tree
* The distance between nodes is computed through the XOR operation (nodes with
    the most bit prefixes in common)
* There are no specialized messages to manage the DHT topology, instead nodes
    rely on all other messages from the protocol to gather and update
    information about the network


High level:
* Nodes are organized in a binary tree according to their identifier prefix
    (some sort of trie map?)
* Each node maintains information about all subtrees where he does not belong
    * You know more about subtrees closer to you than about the ones further
        away
* Routing (or lookup) is performed like other DHTs, in a recursive way
* The lookup can be done several paths in parallel (up to K, where k is the
    amount of nodes you'll contact?)

* Main messages:
    * PING (maybe unneeded)
    * FIND_NODE
    * STORE
    * FIND_VALUE
* All messages carry the identifier of the node sending it (so they can be used
    to manage K-buckets of nodes that receive them)

* Node State:
    * ?
    * ?
    * ?
    <!-- * Each node contains a set of K-buckets -->
    <!-- * Initially each node has a single k-bucket -->
    * When the K bucket is filled, it is broken into two buckets and one will
        never be used again and the other will keep growing

    * At bootstrap, a node will execute a lookup for its own identifier to
        poopulate and initialize its local k-buckets (but this could be
        technically avoided because when something is needed it will bootstrap
        itself?)

---

HyParView
===

Core idea: 2 partial views managed with different strategies.
You only change the partial view when forced (e.g. on join, on death)

[Reactive Strategy]
(Active)
Partial views only updated as a reaction to some external event
Small sized (fanout+1) (which is a limit)
Symmetric

[Cyclic Strategy]
(Passive)
Partial views are updated as a result of some periodically operation
Larger size in order of: k x #(active view)
Just for fault tolerance


How it works:
...


### Partially structured overlay: Plumbtree (to use as a broadcast protocol)

-- Broadcast protocol that builds a topology when broadcasting

Intuition: if we have a connected overlay and we flood a message, we end up with?



