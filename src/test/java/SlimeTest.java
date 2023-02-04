import com.en_circle.slt.plugin.environment.SltLispEnvironment;
import com.en_circle.slt.plugin.environment.SltSBCLEnvironment;
import com.en_circle.slt.plugin.environment.SltSBCLEnvironmentConfiguration;
import com.en_circle.slt.plugin.swank.SlimeListener;
import com.en_circle.slt.plugin.swank.SlimeListener.DebugInterface;
import com.en_circle.slt.plugin.swank.SwankClient;
import com.en_circle.slt.plugin.swank.SwankClient.SwankReply;
import com.en_circle.slt.plugin.swank.SwankPacket;
import com.en_circle.slt.plugin.swank.debug.SltDebugInfo;
import org.awaitility.Awaitility;

import java.math.BigInteger;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

public class SlimeTest {

    public static void main(String[] args) throws Exception {
        try {
            AtomicLong sent = new AtomicLong();
            AtomicLong expected = new AtomicLong();
            SltLispEnvironment environment = new SltSBCLEnvironment();
            environment.start(new SltSBCLEnvironmentConfiguration.Builder().build());
            SlimeListener listener = new SlimeListener(null, false, Throwable::printStackTrace, null, new DebugInterface() {
                @Override
                public void onDebugCreate(SltDebugInfo info) {


                }

                @Override
                public void onDebugActivate(BigInteger debugId, BigInteger level) {

                }

                @Override
                public void onDebugReturn(BigInteger debugId, BigInteger level) {

                }
            });
            try (SwankClient client = new SwankClient("127.0.0.1", 4005, new SwankReply() {
                @Override
                public void onSwankMessage(SwankPacket packet) {
                    listener.onSwankMessage(packet);
                    expected.addAndGet(1);
                }

                @Override
                public void onReadError(Exception e) {
                    e.printStackTrace();
                }
            })) {
                sent.addAndGet(1);
                client.swankSend(SwankPacket.sltEval("(+ + 5)", new BigInteger("3")));

                Awaitility.await()
                        .atMost(10, TimeUnit.SECONDS)
                        .until(() -> expected.get() > sent.get() && sent.get() > 0);
            }
            environment.stop();
        } catch (Exception e) {

        }
    }

}
