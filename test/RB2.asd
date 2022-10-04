Interface:
  Requests:
    rBroadcast(m)
  Indications:
    rBcastDeliver(s,m)

State:
  myself // my own identifier
  correct // correct processes identifiers
  delivered // messages already delivered
  messages  // map that associates to each process p the messages dependent on it

Upon Init(Π, self) do:
  myself <- self
  correct <- Π
  delivered <- {}
  Foreach p ∈ correct do:
    messages[p] <- {} 

Upon rBroadcast(m) do:
  Trigger rBcastDeliver(myself, m)
  delivered <- delivered U {m}
  p <- p ∈ correct: p < p', ∀p' ∈ correct: p /= p' ^ p /= myself
  if p /= ⊥ then
    TriggerSend(BCast, p, self, m)
    messages[p] <- messages[p] U {(myself, m)}

Upon Receive(BCast, s, p, m) do:
  if m ∉ delivered then
    delivered <- delivered U {m}
    d <- d ∈ correct: d > myself ^ d /= p ^ d < d', ∀ d' ∈ correct: d /= d' ^ d' /= p ^ d' > myself
    if d /= ⊥ then
      Trigger Send(BCast, d, p, m)
      messages[d] <- messages[d] U {(p,m)}

Upon Crash(p) do:
  correct <- correct \ {p}
  Foreach (s, m) ∈ messages[p] do:
    d <- d ∈ correct: d > p ^ d /= s ^ d /= myself ^ d < d', ∀ d' ∈ correct: d /= d' ^ d' /= s ^ d' > p
    if d /= ⊥ then
      Trigger Send(BCast, d, s, m)
      messages[d] <- messages[d] U {(s, m)}
