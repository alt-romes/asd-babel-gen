Interface:
    Requests:
        bebBroadcast(m)
    Indications:
        bebDeliver(p, m)

State:
    processes

upon init(ps) do:
    processes <- ps

upon bebBroadcast(m) do:
    foreach q ∈ processes do:
        trigger Send(BroadcastMessage, q, m)

upon receive(BroadcastMessage, q, m) do:
    trigger bebDeliver(q, m)
