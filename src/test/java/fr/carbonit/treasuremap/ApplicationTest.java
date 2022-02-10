package fr.carbonit.treasuremap;

import fr.carbonit.treasuremap.exception.TreasureHuntFile;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.stream.Collectors;

class ApplicationTest {

    @Test
    void should_throw_if_none_input_file_specified() {
        Assertions.assertThatThrownBy(() -> Application.main(new String[1]))
                  .isInstanceOf(TreasureHuntFile.class);
    }

    @Test
    void should_create_file_in_current_folder() {
        // Given
        String[] args = {"src/main/resources/input.txt"};
        // When
        Application.main(args);
        File file = new File("./output.txt");
        // Then
        Assertions.assertThat(file)
                  .exists();

        file.delete();
    }

    @Test
    void should_validate_example() {
        // Given
        String[] args = {"src/main/resources/input.txt"};
        String expected = "C - 3 - 4\n" +
                          "M - 1 - 0\n" +
                          "M - 2 - 1\n" +
                          "T - 0 - 3 - 1\n" +
                          "T - 1 - 3 - 2\n" +
                          "A - Lara - 0 - 3 - S - 3";

        Application.main(args);
        File file = new File("./output.txt");

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
