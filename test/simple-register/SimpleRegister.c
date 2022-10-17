// Intuition:
// I read from my local copy
// Whenever I write, I sned the write to everyone (BEB) and wait for
// confirmations from every correct process
// When I detect a process has crashed, I no longer wait for his confirmations
// When I get confirmations from all correct processes then I complete the write
///

Interface:
    Requests:
        rregRead()
        rregWrite(v)
    Indications:
        rregReadReturn(value)
        rregWriteReturn()
        crash(p)

State:
    value
    writeSet
    correct

upon init(pi) do:
    value <- 0
    writeSet <- {}
    correct <- pi

upon rregRead() do:
    Trigger rregReadReturn(value)

upon rregWrite(v) do:
    Trigger bebBroadcast(v)

upon bebDeliver(s, v) do:
    value <- v
    trigger send(Ack, s, 0) // pp2pSend(Ack, s)

upon receive(Ack, s, i) do:
    writeSet <- writeSet U {s}
    Call checkAcks()

procedure checkAcks() do:
    if correct ⊆ writeSet then:
        writeSet <- {}
        trigger rregWriteReturn()

upon crash(p) do:
    correct <- correct \ {p}
    call checkAcks()

