package fr.carbonit.treasuremap;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

class TreasureMapTest {

    @Test
    void should_return_map_with_width_of_3_cells_and_height_of_4_cells() {
        // Given
        Integer width = 3;
        Integer height = 4;

        String expected = "C - 3 - 4";

        // When
        TreasureMap treasureMap = new TreasureMap(width, height);

        // Then
        Assertions.assertThat(treasureMap).hasToString(expected);
    }

    @Test
    void should_return_map_with_width_of_4_cells_and_height_of_5_cells(){
        // Given
        Integer width = 4;
        Integer height = 5;

        String expected = "C - 4 - 5";

        // When
        TreasureMap treasureMap = new TreasureMap(width, height);

        // Then
        Assertions.assertThat(treasureMap).hasToString(expected);
    }
}
