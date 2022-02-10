package fr.carbonit.treasuremap;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.*;

class TreasureMapTest {
    @Test
    void should_return_map_with_width_of_3_cells_and_height_of_4_cells() {
        // Given
        String  expected  = "C - 3 - 4";
        Integer mapWidth  = 3;
        Integer mapHeight = 4;


        // When
        TreasureMap treasureMap = new TreasureMap(mapWidth, mapHeight);

        // Then
        assertThat(treasureMap.toString())
                  .startsWith(expected);
    }

    @Test
    void should_return_map_with_width_of_4_cells_and_height_of_5_cells() {
        // Given
        Integer mapWidth  = 4;
        Integer mapHeight = 5;

        String expected = "C - 4 - 5";

        // When
        TreasureMap treasureMap = new TreasureMap(mapWidth, mapHeight);

        // Then
        assertThat(treasureMap.toString())
                  .startsWith(expected);
    }

    @Test
    void should_return_map_with_one_mountain_at_0_0() {
        // Given
        Integer mapWidth  = 3;
        Integer mapHeight = 4;

        Integer mountainXCoordinate = 0;
        Integer mountainYCoordinate = 0;

        String expected = "C - 3 - 4\nM - 0 - 0";

        // When
        TreasureMap treasureMap = new TreasureMap(mapWidth, mapHeight);
        treasureMap.addMountain(mountainXCoordinate, mountainYCoordinate);


        // Then
        assertThat(treasureMap.toString())
                  .startsWith(expected
                             );
    }

    @Test
    void should_return_map_with_one_mountain_at_1_1() {
        // Given
        Integer mapWidth  = 3;
        Integer mapHeight = 4;

        Integer mountainXCoordinate = 1;
        Integer mountainYCoordinate = 1;

        String expected = "C - 3 - 4\nM - 1 - 1";

        // When
        TreasureMap treasureMap = new TreasureMap(mapWidth, mapHeight);
        treasureMap.addMountain(mountainXCoordinate, mountainYCoordinate);


        // Then
        assertThat(treasureMap.toString())
                  .startsWith(expected
                             );
    }

    @Test
    void should_return_map_with_one_mountain_at_1_1_and_one_mountain_at_0_0() {
        // Given
        Integer mapWidth  = 3;
        Integer mapHeight = 4;

        String expected = "C - 3 - 4\nM - 0 - 0\nM - 1 - 1";

        // When
        TreasureMap treasureMap = new TreasureMap(mapWidth, mapHeight);
        treasureMap.addMountain(1, 1);
        treasureMap.addMountain(0, 0);


        // Then
        assertThat(treasureMap.toString())
                  .startsWith(expected
                             );
    }

    @Test
    void should_return_map_with_one_mountain_at_1_1_and_treasure_at0_0() {
        // Given
        Integer mapWidth  = 3;
        Integer mapHeight = 4;

        String expected = "C - 3 - 4\nM - 1 - 1\nT - 0 - 0 - 1";

        // When
        TreasureMap treasureMap = new TreasureMap(mapWidth, mapHeight);
        treasureMap.addMountain(1, 1);
        treasureMap.addTreasure(0, 0);


        // Then
        assertThat(treasureMap)
                  .hasToString(expected
                              );
    }

    @Test
    void should_return_map_with_one_mountain_at_1_1_and_2_treasures_at0_0() {
        // Given
        Integer mapWidth  = 3;
        Integer mapHeight = 4;

        String expected = "C - 3 - 4\nM - 1 - 1\nT - 0 - 0 - 2";

        // When
        TreasureMap treasureMap = new TreasureMap(mapWidth, mapHeight);
        treasureMap.addMountain(1, 1);
        treasureMap.addTreasure(0, 0);
        treasureMap.addTreasure(0, 0);


        // Then
        assertThat(treasureMap)
                .hasToString(expected
                            );
    }


}
