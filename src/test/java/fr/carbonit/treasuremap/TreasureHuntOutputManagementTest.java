package fr.carbonit.treasuremap;

import fr.carbonit.treasuremap.treasurehunt.TreasureHunt;
import fr.carbonit.treasuremap.treasurehunt.TreasureHuntOutputManagement;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.stream.Collectors;

@ExtendWith(MockitoExtension.class)
class TreasureHuntOutputManagementTest {

    @Mock
    TreasureHunt treasureHunt;

    @InjectMocks
    TreasureHuntOutputManagement treasureHuntOutputManagement;

    @Test
    void should_write_file_to_outputFileFolder() {
        // Given
        String filePath = "./output.txt";
        // When
        String expected = "TREASURE HUNT FINISHED\nEND";
        Mockito.when(treasureHunt.getSimulationResult())
               .thenReturn(expected);
        treasureHuntOutputManagement.write();
        File file = new File(filePath);
        // Then
        Assertions.assertThat(file)
                  .exists()
                  .isNotEmpty();

        try {
            BufferedReader reader = new BufferedReader(new FileReader(file));
            Assertions.assertThat(reader.lines()
                                        .collect(Collectors.joining()))
                      .isEqualTo(expected.replace("\n", ""));
        } catch (FileNotFoundException fileNotFoundException) {
            fileNotFoundException.printStackTrace();
        } finally {
            file.delete();
        }
    }

}
