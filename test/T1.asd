Interface:
    Requests:
        fp2pSend(p, m)
    Indications:
        fp2pDeliver(s, m)

    Upon fp2pSend(p, m) do
        Trigger Send(DATA, p, m)
    done

    Upon Receive(DATA, s, m) do
        Trigger fp2pDeliver(s,m)
    done

