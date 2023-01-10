import com.en_circle.slt.plugin.swank.*;
import org.awaitility.Awaitility;

import java.math.BigInteger;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

public class SlimeTest {

    public static void main(String[] args) throws Exception {
        try {
            AtomicLong sent = new AtomicLong();
            AtomicLong expected = new AtomicLong();
            SwankServer.startSbcl(new SwankServerConfiguration.Builder().build());
            SlimeListener listener = new SlimeListener(null, false, null);
            try (SwankClient client = new SwankClient("127.0.0.1", 4005, packet -> {
                listener.onSwankMessage(packet);
                expected.addAndGet(1);
            })) {
                sent.addAndGet(1);
                client.swankSend(SwankPacket.sltEval("(+ + 5)", new BigInteger("3")));

                Awaitility.await()
                        .atMost(10, TimeUnit.SECONDS)
                        .until(() -> expected.get() > sent.get() && sent.get() > 0);
            }
            SwankServer.stop();
        } catch (Exception e) {

        }
    }

}
