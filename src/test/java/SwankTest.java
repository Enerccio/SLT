import com.en_circle.slt.plugin.swank.SwankClient;
import com.en_circle.slt.plugin.swank.SwankServer;
import org.awaitility.Awaitility;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

public class SwankTest {

    public static void main(String[] args) throws Exception {
        try {
            AtomicLong sent = new AtomicLong();
            AtomicLong expected = new AtomicLong();
            SwankServer.startSbcl("sbcl", 4005);
            try (SwankClient client = new SwankClient("127.0.0.1", 4005, packet -> {
                System.out.println(packet);
                expected.addAndGet(1);
            })) {
//                sent.addAndGet(1);
//                client.swankSend(new SlimePacket("(:return (:ok nil) 1)"));
//                sent.addAndGet(1);
//                client.swankSend(SlimePacket.rpcReturnOk("(+ 1 2)", 2));
//                sent.addAndGet(1);
//                client.swankSend(SlimePacket.rpcWriteString("(+ 4 5)"));
//                sent.addAndGet(1);
//                client.swankSend(SlimePacket.rpcNewPackage("cl-user"));

                Awaitility.await()
                        .atMost(10, TimeUnit.SECONDS)
                        .until(() -> expected.get() > sent.get() && sent.get() > 0);
            }
            SwankServer.stop();
        } catch (Exception e) {

        }
    }

}
