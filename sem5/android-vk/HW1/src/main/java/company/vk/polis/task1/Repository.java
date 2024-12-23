package company.vk.polis.task1;

import java.util.List;

public interface Repository {
    static List<Entity> getInfo() {
        return DataUtils.generateEntity();
    }
}
