State:
  myself
  neighbours
  received
  channelReady

Upon Init(self) do:
  myself     <- self
  neighbours <- {}
  received   <- {}
  channelReady <- false

Upon ChannelCreated() do:
  // We should register our own callbacks and serializers, only then,
  channelReady <- true

Upon broadcastRequest(mid, s, m) do:
  If channelReady Then
    Trigger floodMessage(mid, s, m, myself)

Upon floodMessage(mid, s, m, from) do:
  If mid ∉ received Then
    received <- received U {mid}
    ; Trigger Notify(DeliverNotification, mid, s, m) // Special syntax for cross protocol notifications?
    Foreach host ∈ neighbours do:
      If host /= from Then
        Trigger Send(FloodMessage, host, mid, s)

Upon neighbourUp(upNeighbours) do:
  Foreach h ∈ upNeighbours do:
    neighbours <- neighbours U {h}

Upon neighbourDown(downNeighbours) do:
  Foreach h ∈ downNeighbours do:
    neighbours <- neighbours \ {h}
