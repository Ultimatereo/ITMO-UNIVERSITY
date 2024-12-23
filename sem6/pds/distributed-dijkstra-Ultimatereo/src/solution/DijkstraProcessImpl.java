package solution;

import internal.Environment;

/**
 * Distributed Dijkstra algorithm implementation.
 * All functions are called from the single main thread.
 *
 * @author <First-Name> <Last-Name> // todo: replace with your name
 */
public class DijkstraProcessImpl implements DijkstraProcess {
    record Message(int type, long dist) implements java.io.Serializable {
    }

    private static final int hello = 0, ack = 1, goodbye = 2;
    private static final Message ackAccept = new Message(ack, 1);
    private static final Message ackReject = new Message(ack, -1);
    private final Environment env;
    private int parent = -1;
    private int childCount = 0;
    private int balance = 0;
    private long dist = Long.MAX_VALUE;

    public DijkstraProcessImpl(Environment env) {
        this.env = env;
    }

    @Override
    public void onMessage(int senderPid, Object message) {
        if (message instanceof Message msg) {
            if (msg.type == hello) {
                if (msg.dist < dist) {
                    dist = msg.dist;
                    if (broadcast()) {
                        if (parent == -1) {
                            parent = senderPid;
                            env.send(senderPid, ackAccept);
                        } else {
                            env.send(senderPid, ackReject);
                        }
                    } else {
                        env.send(senderPid, ackReject);
                    }
                } else {
                    env.send(senderPid, ackReject);
                }
            } else if (msg.type == ack) {
                balance--;
                if (msg == ackAccept) {
                    childCount++;
                } else {
                    tryLeave();
                }
            } else if (msg.type == goodbye) {
                childCount--;
                tryLeave();
            }
        }
    }

    private void tryLeave() {
        if (childCount == 0 && balance == 0) {
            if (parent == -1) {
                env.finishExecution();
                return;
            }
            env.send(parent, new Message(goodbye, dist));
            parent = -1;
        }
    }

    @Override
    public Long getDistance() {
        return dist == Long.MAX_VALUE ? null : dist;
    }

    @Override
    public void onComputationStart() {
        dist = 0;
        broadcast();
        tryLeave();
    }

    private boolean broadcast() {
        boolean sent = false;
        for (int neighbor : env.getNeighbours().keySet()) {
            if (neighbor != env.getProcessId()) {
                balance++;
                sent = true;
                env.send(
                        neighbor,
                        new Message(
                                DijkstraProcessImpl.hello,
                                dist + env.getNeighbours().get(neighbor)
                        )
                );
            }
        }
        return sent;
    }
}
