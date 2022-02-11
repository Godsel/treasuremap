package fr.carbonit.treasuremap;

import fr.carbonit.treasuremap.exception.DataValidity;
import fr.carbonit.treasuremap.exception.TreasureHuntFile;
import fr.carbonit.treasuremap.treasurehunt.TreasureHunt;
import fr.carbonit.treasuremap.treasurehunt.TreasureHuntInputManagement;
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

    @Test
    void should_return_treasureMap_object() {
        // Given
        String argFilePath = null;
        TreasureHuntInputManagement
                treasureHuntInputManagement = new TreasureHuntInputManagement("src/main/resources/input.txt");
        // When

        TreasureHunt read = treasureHuntInputManagement.read();

        // Then

        Assertions.assertThat(read)
                  .isNotNull();
    }

    @Test
    void should_throw_when_unsupportedAdventurerActions() {
        // Given
        String argFilePath = null;
        TreasureHuntInputManagement
                treasureHuntInputManagement = new TreasureHuntInputManagement(
                "src/test/resources/testFiles/unsupportedAdventurerActions.txt");
        // When
        Assertions.assertThatThrownBy(treasureHuntInputManagement::read)
                  .isInstanceOf(DataValidity.class)
                  .hasMessage("Unsupported Adventurer Actions");

    }

    @Test
    void should_throw_when_unsupportedOrientation() {
        // Given
        String argFilePath = null;
        TreasureHuntInputManagement
                treasureHuntInputManagement = new TreasureHuntInputManagement(
                "src/test/resources/testFiles/unsupportedOrientation.txt");
        // When
        Assertions.assertThatThrownBy(treasureHuntInputManagement::read)
                  .isInstanceOf(DataValidity.class)
                  .hasMessage("Unsupported Orientation");

    }
}
