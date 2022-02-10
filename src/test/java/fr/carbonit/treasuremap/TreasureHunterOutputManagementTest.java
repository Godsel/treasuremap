package fr.carbonit.treasuremap;

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
class TreasureHunterOutputManagementTest {

    @Mock
    TreasureHunt treasureHunt;

    @InjectMocks
    TreasureHunterOutputManagement treasureHunterOutputManagement;

    @Test
    void should_write_file_to_outputFileFolder() {
        // Given
        String filePath = "src/main/resources/output.txt";
        // When
        String expected = "TREASURE HUNT FINISHED\nEND";
        Mockito.when(treasureHunt.getSimulationResult())
               .thenReturn(expected);
        treasureHunterOutputManagement.write();
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
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } finally {
            file.delete();
        }
    }

}
