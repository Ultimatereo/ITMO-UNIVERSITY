#include <algorithm>
#include <chrono>
#include <fstream>
#include <iostream>
#include <map>
#include <numeric>
#include <random>
#include <string>
#include <vector>


class TrellisNode;

using namespace std;

double nextGaussian(double mean, double sigma) {
    static default_random_engine generator(chrono::system_clock::now().time_since_epoch().count());
    normal_distribution<double> normalDistribution(mean, sigma);
    return normalDistribution(generator);
}

class TrellisNode {
public:
    TrellisNode *zero = nullptr;
    TrellisNode *one = nullptr;
    std::vector<std::pair<bool, int>> code;
    double distance = -1;
    TrellisNode *prev = nullptr;
};

class Decoder {
public:
    Decoder(const unsigned int &countOfRows, const unsigned int &countOfColumns, const std::vector<std::vector<bool>> &matrix) {
        this->countOfColumns = countOfColumns;
        this->countOfRows = countOfRows;
        this->matrix = matrix;
        init();
    }

    std::vector<bool> encode(const std::vector<bool> &input) {
        std::vector<bool> result(this->countOfColumns);
        unsigned int i = 0;
        while(i < this->countOfColumns){
             result[i] = accumulate(begin(matrix), end(matrix), false,
                                   [&](bool accum, const std::vector<bool> &row) { return accum ^= (row[i] & input[&row - &matrix[0]]); });
            i++;
        }

        return result;
    }

    std::vector<bool> decode(const std::vector<double> &signal) {
        graphLayers[0][0]->distance = 0;

        for_each(begin(graphLayers) + 1, end(graphLayers),
                 [&](const std::vector<TrellisNode *> &layer) {
                     for_each(begin(layer), end(layer),
                              [](TrellisNode *node) {
                                  node->distance = DEFAULT_DISTANCE_VALUE;
                                  node->prev = nullptr;
                              });
                 });

       int i = 0;
        while(i < signal.size()){
              const double value = signal[i];
            for_each(begin(graphLayers[i]), end(graphLayers[i]),
                     [&](TrellisNode *node) {
                         if (node->zero != nullptr) {
                             updateDistance(node, node->zero, -value);
                         }
                         if (node->one != nullptr) {
                             updateDistance(node, node->one, value);
                         }
                     });
            i++;
        }

        std::vector<bool> answer;
        TrellisNode *current = graphLayers[graphLayers.size() - 1][0];

         unsigned int layer = graphLayers.size() - 1;
        while (layer >= 1 && current) {
            TrellisNode *prev = current->prev;
            if (prev && prev->zero == current) {
                answer.push_back(false);
            } else {
                answer.push_back(true);
            }
            current = prev;
            layer--;
        }

        reverse(answer.begin(), answer.end());
        return answer;
    }

    double simulate(const double &noise, const unsigned int &maxIterations, const unsigned int &maxErrors) {
        unsigned int mistakes = 0;
        unsigned int iterations = 0;

        std::vector<bool> vec(this->countOfColumns);

        unsigned int i = 0;
         while(i < maxIterations){
            iterations++;
            randomFill(vec);
            std::vector<bool> encoded = this->encode(vec);
            std::vector<double> signal = this->sendSignal(encoded, noise);
            std::vector<bool> decoded = this->decode(signal);

            if (decoded != encoded) {
                mistakes++;
            }

            if (mistakes == maxErrors) {
                break;
            }
            i++;
        }

        return double(mistakes) / iterations;
    }

    std::vector<unsigned int> sizes() {
        std::vector<unsigned int> result(graphLayers.size());
        transform(begin(graphLayers), end(graphLayers), begin(result),
                  [](const std::vector<TrellisNode *> &layer) {
                      return layer.size();
                  });
        return result;
    }

private:
    static const int DEFAULT_DISTANCE_VALUE = -1;

    unsigned int countOfRows;
    unsigned int countOfColumns;
    std::vector<std::vector<bool>> matrix;
    std::vector<std::vector<TrellisNode *>> graphLayers;

    static void updateDistance(TrellisNode *current, TrellisNode *next, const double &distance) {
        if (next->distance == DEFAULT_DISTANCE_VALUE || next->distance > current->distance + distance) {
            next->distance = current->distance + distance;
            next->prev = current;
        }
    }

    static void randomFill(std::vector<bool> &vec) {
        mt19937 getRandom((random_device())());
       size_t i = 0;
        while(i < vec.size()){
            vec[i] = getRandom() % 2;
            i++;
        }
    }

    std::vector<double> sendSignal(const std::vector<bool> &vec, const double &noise) const {
        std::vector<double> result(vec.size());
        double decibel = pow(10, -noise / 10);
        double deviation = (0.5 * this->countOfColumns / this->countOfRows) * decibel;
        double sigma = sqrt(deviation);

        transform(begin(vec), end(vec), begin(result), [&](bool bit) {
            return (bit ? -1.0 : 1.0) + nextGaussian(0.0, sigma);
        });

        return result;
    }

    void init() {
        std::vector<std::vector<bool>> msfMatrix = getMinimalSpanFormMatrix();
        std::vector<std::pair<unsigned int, unsigned int>> spins = getSpins(msfMatrix);

        const unsigned int rowsCount = this->countOfRows;
        const unsigned int columnCount = this->countOfColumns;

        auto *start = new TrellisNode();
        std::vector<std::vector<TrellisNode *>> layers(columnCount + 1);
        std::vector<TrellisNode *> prevLayer;
        prevLayer.push_back(start);
        layers[0].push_back(start);

         unsigned int i = 1;
        while(i < columnCount + 1){
              unsigned int matrixColumn = i - 1;
            std::vector<bool> column(rowsCount);

             int j = 0;
            while(j < rowsCount){
                  column[j] = msfMatrix[j][matrixColumn];
                 j++;
            }

            int firstActiveLayer = -1;
            std::vector<unsigned int> activeLayers;

             j = 0;
           while(j < rowsCount){
                  if (matrixColumn == spins[j].first) {
                    firstActiveLayer = j;
                }
                if (matrixColumn >= spins[j].first && matrixColumn <= spins[j].second) {
                    activeLayers.push_back(j);
                }
              j++;
            }
            std::map<std::vector<std::pair<bool, int>>, TrellisNode *> nextLayer;

            for_each(begin(prevLayer), end(prevLayer),
                     [&](TrellisNode *node) {
                         if (firstActiveLayer == -1) {
                             std::vector<std::pair<bool, int>> nNode = node->code;
                             bool edge = mul(column, nNode);
                             addEdge(nNode, spins, node, edge, nextLayer, matrixColumn);
                             return;
                         }

                         std::vector<std::pair<bool, int>> zero = node->code;
                         std::vector<std::pair<bool, int>> one = node->code;

                         zero.emplace_back(0, firstActiveLayer);
                         one.emplace_back(1, firstActiveLayer);

                         bool onOne = mul(column, one);

                         addEdge(zero, spins, node, !onOne, nextLayer, matrixColumn);
                         addEdge(one, spins, node, onOne, nextLayer, matrixColumn);
                     });

            prevLayer.clear();
            for (const auto &cur: nextLayer) {
                prevLayer.push_back(cur.second);
            }

            layers[i] = prevLayer;
            i++;
        }
        this->graphLayers = layers;
    }

    std::vector<std::pair<unsigned int, unsigned int>> getSpins(const std::vector<std::vector<bool>> &msfMatrix) const {
        const unsigned int rowsCount = this->countOfRows;
        const unsigned int columnCount = this->countOfColumns;
        std::vector<std::pair<unsigned int, unsigned int>> spins;

        int i = 0;
         while(i < rowsCount){
            unsigned int start = 0;
            unsigned int end = columnCount - 1;

             while (!msfMatrix[i][start]) start++;
            while (!msfMatrix[i][end]) end--;

            if (start < end) end--;
            else
                end = start;

            spins.emplace_back(start, end);
            i++;
        }

        return spins;
    }

    std::vector<std::vector<bool>> getMinimalSpanFormMatrix() {
        std::vector<std::vector<bool>> gMatrix = this->matrix;
        const unsigned int rowsCount = this->countOfRows;
         unsigned int column = 0;

        unsigned int rowCurrent = 0;
         while(rowCurrent < rowsCount){
           bool isClear = true;

            if (!gMatrix[rowCurrent][column]) {
                isClear = false;
                 unsigned int row = rowCurrent + 1;
                while(row < rowsCount){
                     if (gMatrix[row][column]) {
                        xorRows(row, rowCurrent, gMatrix);
                        isClear = true;
                        break;
                    }
                    row++;
                }
            }
                if (isClear) {
                     unsigned int row = rowCurrent + 1;
                    while(row < rowsCount){
                           if (gMatrix[row][column]) {
                            xorRows(rowCurrent, row, gMatrix);
                         }
                        row++;
                    }
                } else {
                rowCurrent--;
            }
             column++;
            rowCurrent++;
        }

        std::vector<bool> free(rowsCount, true);
        column = this->countOfColumns - 1;

        unsigned int step = 0;
         while(step < rowsCount){
            int count = 0;
             unsigned int i = 0;
           while(i < rowsCount){
                  if (free[i] && gMatrix[i][column]) {
                    count++;
                }
               i++;
            }

            if (count > 0) {
                unsigned int last = rowsCount - 1;
                while (!(gMatrix[last][column] && free[last])) {
                    last--;
                }
                free[last] = false;
                if (count > 1) {
                    unsigned int i = 0;
                 while(i < last){
                       if (gMatrix[i][column] && free[i]) {
                            xorRows(last, i, gMatrix);
                        }
                     i++;
                    }
                }
            } else {
                step--;
            }
             column--;
             step++;
        }
        return gMatrix;
    }

    static TrellisNode *getNodesById(
            std::map<std::vector<std::pair<bool, int>>, TrellisNode *> &gr,
            const std::vector<std::pair<bool, int>> &key) {
        if (gr.count(key) != 0) {
            return gr[key];
        }

        auto *newNode = new TrellisNode();
        newNode->code = key;
        gr[key] = newNode;
        return newNode;
    }

    static void addEdge(
            std::vector<std::pair<bool, int>> &code,
            const std::vector<std::pair<unsigned int, unsigned int>> &spins,
            TrellisNode *node,
            bool positive,
            std::map<std::vector<std::pair<bool, int>>, TrellisNode *> &nextLayer,
            const unsigned int matrixColumn) {
        rmPosition(code, spins, matrixColumn);
        TrellisNode *newNode = getNodesById(nextLayer, code);
        if (positive) {
            node->one = newNode;
        } else {
            node->zero = newNode;
        }
    }

    static void rmPosition(std::vector<std::pair<bool, int>> &vec, const std::vector<std::pair<unsigned int, unsigned int>> &spins, const unsigned int pos) {
           int i = 0;
        while (i < vec.size()) {
            if (pos > spins[vec[i].second].second) {
                vec.erase(vec.begin() + i);
                break;
            }
            i++;
        }
    }

    static bool mul(const std::vector<bool> &prevColumn, const std::vector<std::pair<bool, int>> &nextLayer) {
        return accumulate(begin(nextLayer), end(nextLayer), false,
                          [&](bool result, const std::pair<bool, int> &p) {
                              return result ^= (p.first && prevColumn[p.second]);
                          });
    }

    void xorRows(const unsigned int &row1, const unsigned int &row2, std::vector<std::vector<bool>> &m) const {
        size_t i = 0;
        while (i < m[row2].size()) {
            m[row2][i] = m[row2][i] ^ m[row1][i];
             i++;
        }
    }
};

int main() {
    freopen("input.txt", "r", stdin);
    freopen("output.txt", "w", stdout);

    unsigned int countOfColumns, countOfRows;
    cin >> countOfColumns >> countOfRows;

    std::vector<std::vector<bool>> matrix(countOfRows, std::vector<bool>(countOfColumns, false));

    for (int i = 0; i < countOfRows; i++) {
        for (int j = 0; j < countOfColumns; j++) {
            unsigned int val;
            cin >> val;
            matrix[i][j] = (val == 1);
        }
    }

    Decoder decoder(countOfRows, countOfColumns, matrix);
    std::vector<unsigned int> levels = decoder.sizes();

    for_each(begin(levels), end(levels), [&](unsigned int level) {
        cout << level << " ";
    });

    string command;
    double noiseLevel;
    unsigned int iterations, errors;
    std::vector<bool> input = std::vector<bool>(countOfRows);
    std::vector<double> signal = std::vector<double>(countOfColumns);

    while (cin >> command) {
        cout << "\n";

         switch (command[0]) {
            case 'E': {
               int i = 0;
               while(i < countOfRows){
                   unsigned int val;
                   cin >> val;
                   input[i] = val == 1;
                   i++;
               }

               std::vector<bool> result = decoder.encode(input);
               for_each(begin(result), end(result), [&](bool val) {
                   cout << val << " ";
               });
               continue;
            }
            case 'D': {
              int i = 0;
               while(i < countOfColumns){
                  cin >> signal[i];
                  i++;
               }

              std::vector<bool> result = decoder.decode(signal);
              for_each(begin(result), end(result), [&](bool val) {
                    cout << val << " ";
                });
             continue;
            }
            case 'S': {
                cin >> noiseLevel >> iterations >> errors;
                cout << decoder.simulate(noiseLevel, iterations, errors);
            }
         }
    }
    return 0;
}