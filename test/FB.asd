Interface:
  Requests:
    broadcastRequest

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
  ; We should register our own callbacks and serializers, only then,
  channelReady <- true

Upon broadcastRequest(mid, s, m) do:
  If channelReady Then
    Trigger Send(FloodMessage, myself, mid, s, m)

Upon Receive(FloodMessage, from, mid, s, m) do:
  If mid ∉ received Then
    received <- received U {mid}
    Trigger deliverNotification(mid, s, m)
    Foreach host ∈ neighbours do:
      If host /= from Then
        Trigger Send(FloodMessage, host, mid, s, m)

Upon neighbourUp(upNeighbours) do:
  Foreach h ∈ upNeighbours do:
    neighbours <- neighbours U {h}

Upon neighbourDown(downNeighbours) do:
  Foreach h ∈ downNeighbours do:
    neighbours <- neighbours \ {h}
