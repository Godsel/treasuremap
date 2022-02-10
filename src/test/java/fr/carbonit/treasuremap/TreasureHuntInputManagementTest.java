package fr.carbonit.treasuremap;

import fr.carbonit.treasuremap.exception.TreasureHuntFile;
import fr.carbonit.treasuremap.file.TreasureHuntInputManagement;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

class TreasureHuntInputManagementTest {
    @Test
    void should_throw_if_file_does_not_exist() {
        // Given
        String                      argFilePath                 = null;
        TreasureHuntInputManagement treasureHuntInputManagement = new TreasureHuntInputManagement("");
        // When

        // Then
        Assertions.assertThatThrownBy(treasureHuntInputManagement::read)
                  .isInstanceOf(TreasureHuntFile.class);
    }
}
