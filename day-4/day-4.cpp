#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <sstream>
#include <algorithm>

std::vector<std::string> getInput() {
    std::ifstream file("input.txt");
    std::vector<std::string> input;
    std::string line;

    while (file >> line) input.push_back(line);

    return input;
}

std::vector<std::string> transpose(std::vector<std::string> &data)  {
    size_t newSize = data[0].length();
    std::vector<std::string> transposed(newSize);

    for (size_t i = 0; i < newSize; i++) {
        std::stringstream temp;
        for (size_t j = 0; j < data.size(); j++) temp << data[j][newSize - i - 1];
        transposed[i] = temp.str();
    }

    return transposed;
};


int partOne(std::vector<std::string> &input) {
    auto transposed = transpose(input);

    auto countMatches = [](std::vector<std::string> &data) -> unsigned int {
        unsigned int counts = 0;
        for (size_t i = 0; i < data.size(); i++) {
            size_t pos = 0;
            while ((pos = data[i].find("XMAS", pos)) != std::string::npos) { counts++; pos += 4; }

            pos = 0;
            while ((pos = data[i].find("SAMX", pos)) != std::string::npos) { counts++; pos += 4; }
        }
        return counts;
    };

    auto countDiagonalMatches = [&countMatches](std::vector<std::string> &data) -> unsigned int {
        std::vector<std::string> diagonals;

        for (size_t i = 0; i <= data.size() - 4; i++) {
            std::stringstream diagonal;

            if (i != 0) {
                for (size_t j = i; j < data[0].length(); j++) diagonal << data[j - i][j];
                diagonals.push_back(diagonal.str()); diagonal.str("");
            }

            for (size_t j = i; j < data[0].length(); j++) diagonal << data[j][j - i];

            diagonals.push_back(diagonal.str());
        }

        return countMatches(diagonals);
    };

    return countMatches(input) +
           countMatches(transposed) +
           countDiagonalMatches(input) +
           countDiagonalMatches(transposed);
}

int main() {
    auto input = getInput();

    std::cout << "Part One: " << partOne(input) << std::endl;

    return 0;
}