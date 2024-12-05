#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <sstream>
#include <algorithm>

std::vector<std::string> getInput() {
    std::ifstream file("day-4/input.txt");
    std::vector<std::string> input;
    std::string line;

    while (file >> line) input.push_back(line);

    return input;
}

std::vector<std::string> transpose(const std::vector<std::string> &data)  {
    const size_t newSize = data[0].length();
    std::vector<std::string> transposed(newSize);

    std::stringstream builder;
    for (size_t i = 0; i < newSize; i++) {
        for (size_t j = 0; j < data.size(); j++) builder << data[j][newSize - i - 1];
        transposed[i] = builder.str(); builder.str("");
    }

    return transposed;
};

unsigned int countMatches(const std::vector<std::string> &data, const std::string &match) {
    unsigned int counts = 0;
    std::string reverse(match); std::reverse(reverse.begin(), reverse.end());

    for (size_t i = 0; i < data.size(); i++) {
        size_t pos = 0;
        while ((pos = data[i].find(match, pos)) != std::string::npos) { counts++; pos += match.length(); }

        pos = 0;
        while ((pos = data[i].find(reverse, pos)) != std::string::npos) { counts++; pos += match.length(); }
    }
    return counts;
};

unsigned int countDiagonalMatches(const std::vector<std::string> &data, const std::string &match) {
    std::vector<std::string> diagonals;

    std::stringstream diagonal;
    for (size_t i = 0; i <= data.size() - match.length(); i++) {
        for (size_t j = i; j < data[0].length(); j++) diagonal << data[j][j - i];
        diagonals.push_back(diagonal.str()); diagonal.str("");

        if (i == 0) continue;
        for (size_t j = i; j < data[0].length(); j++) diagonal << data[j - i][j];
        diagonals.push_back(diagonal.str()); diagonal.str("");
    }

    return countMatches(diagonals, match);
};


unsigned int partOne(const std::vector<std::string> &input) {
    auto transposed = transpose(input);
    std::string match("XMAS");

    return countMatches(input, match) +
           countMatches(transposed, match) +
           countDiagonalMatches(input, match) +
           countDiagonalMatches(transposed, match);
}

unsigned int partTwo(const std::vector<std::string> &input) {
    const std::string match("MAS");
    size_t matchSize = match.length();
    size_t counts = 0;

    std::vector<std::string> partition(matchSize);
    for (size_t i = 0; i <= input[0].size() - matchSize; i++) {
        for (size_t j = 0; j <= input[0].size() - matchSize; j++) {
            std::generate(partition.begin(), partition.end(), [&input, i, j, matchSize, n = 0]() mutable -> std::string {
                return input[i + n++].substr(j, matchSize);
            });
            counts += countDiagonalMatches(partition, match) & countDiagonalMatches(transpose(partition), match);
        }
    }

    return counts;
}

int main() {
    auto input = getInput();

    std::cout << "Part One: " << partOne(input) << "\nPart Two: " << partTwo(input) << std::endl;

    return 0;
}