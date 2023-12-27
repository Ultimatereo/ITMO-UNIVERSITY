#include <iostream>
#include <omp.h>
#include <stdio.h>
#include <vector>
#include <fstream>

void getMinMax(float coef, const std::vector<unsigned char> &file, int startOfBytes, int &amin, int &amax,
               unsigned char type, int s);

int getMax(const unsigned int *R, unsigned int ignor);

int getMin(unsigned int R[256], unsigned int ignor);

using namespace std;

std::vector<unsigned char> readFile(const std::string &filename) {
    std::streampos fileSize;
    std::ifstream file(filename, std::ios::binary);
    file.seekg(0, std::ios::end);
    fileSize = file.tellg();
    file.seekg(0, std::ios::beg);
    std::vector<unsigned char> fileData(fileSize);
    file.read((char *) &fileData[0], fileSize);
    return fileData;
}

int main(int argc, char **argv) {
    if (argc < 5) {
        fprintf(stderr, "You should type 4 arguments! Number of threads, inputFile, outputFile and coefficient!");
        exit(0);
    }
    int num_of_threads = atoi(argv[1]);
    if (num_of_threads < 0) {
        fprintf(stderr, "Number of threads should be more or equal zero!");
        exit(0);
    }
    omp_set_num_threads(atoi(argv[1]));
    std::string inputFile(argv[2]);
    std::string outputFile(argv[3]);
    float coef = std::stof(argv[4]);
    if (coef < 0 or coef >= 0.5) {
        fprintf(stderr, "Coefficient should be between 0 and 0.5!");
        exit(0);
    }

    //printf("%d %s %s %f\n", num_of_threads, inputFile.c_str(), outputFile.c_str(), coef);
    vector<unsigned char> file = readFile(inputFile);
    double start_time = omp_get_wtime();
    if (file.size() == 0) {
        fprintf(stderr, "File is empty!");
        exit(0);
    }
    if (file[0] != 'P') {
        fprintf(stderr, "File's magic numbers should start with 'P'");
        exit(0);
    }
    if (file[1] != '5' && file[1] != '6') {
        fprintf(stderr, "File's magic numbers should end with '5' or '6'");
        exit(0);
    }
    unsigned char type = file[1];
    if (file[2] != 0x0A) {
        fprintf(stderr, "Line separator is missing");
    }
    int index = 3;
    string width;
    while (file[index] != 0x20) {
        width += file[index++];
    }
    index++;
    string height;
    while (file[index] != 0x0A) {
        height += file[index++];
    }
    index++;
    string pixels;
    //printf("width: %s and height: %s. And of course type: %c\n", width.c_str(), height.c_str(), type);
    while (file[index] != 0x0A) {
        pixels += file[index++];
    }
    if (pixels != "255") {
        fprintf(stderr, "This program only works with 255 pixels rad. actual: %s", pixels.c_str());
        exit(0);
    }
    index++;
    int startOfBytes = index;
    int amin;
    int amax;
    getMinMax(coef, file, startOfBytes, amin, amax, type, num_of_threads);
    if (amin == amax) {
        fprintf(stderr, "You have overdone contrast! Please make coef less!");
        exit(0);
    }
    //printf("amin : %d, amax: %d\n", amin, amax);
    float cc = 255.0 * 1 / (amax - amin);
#pragma omp parallel
    {
#pragma omp for schedule(dynamic, 512)
//#pragma omp for schedule(static, 512)
        for (int i = startOfBytes; i < file.size(); i++) {
            int answer = (file[i] - amin) * cc;
            if (answer < 0) {
                file[i] = 0;
            } else if (answer > 255) {
                file[i] = 255;
            } else {
                file[i] = answer;
            }
        }
    }
    printf("Time (%i thread(s)): %g ms\n", num_of_threads, (omp_get_wtime() - start_time) * 1000);
    ofstream fout(outputFile, std::ios::binary);
    for (unsigned char i: file) {
        fout << i;
    }
}


void getMinMax(float coef, const vector<unsigned char> &file, int startOfBytes, int &amin, int &amax, unsigned char type,
          int num_of_threads) {
    if (type == '5') {
        unsigned int bytes[256] = {};
        int numOfBytes = file.size() - startOfBytes;
#pragma omp parallel
        {
#pragma omp for schedule(dynamic, 512) reduction(+:bytes)
//#pragma omp for schedule(static, 512) reduction(+:bytes)
            for (int i = startOfBytes; i < file.size(); i++) {
//#pragma omp atomic
                bytes[file[i]] += 1;
            }
        }
        //printf("Number of total bytes: %d\nAnd fucking bytes: \n", numOfBytes);
        unsigned int ignor = coef * numOfBytes;
        //printf("We should ignore %d bytes\n", ignor);
        amin = getMin(bytes, ignor);
        amax = getMax(bytes, ignor);
    } else if (type == '6') {
        unsigned int R[256] = {};
        unsigned int G[256] = {};
        unsigned int B[256] = {};
        int numOfBytes = file.size() - startOfBytes;
#pragma omp parallel
        {
#pragma omp for schedule(dynamic, 512) reduction(+:R, G, B)
//#pragma omp for schedule(static, 512) reduction(+:R, G, B)
            for (int i = startOfBytes; i < file.size(); i += 3) {
//#pragma omp atomic
                R[file[i]] += 1;
//#pragma omp atomic
                G[file[i + 1]] += 1;
//#pragma omp atomic
                B[file[i + 2]] += 1;
            }
        }

        //printf("Number of total bytes: %d\nAnd fucking bytes: \n", numOfBytes);
        //        for (int i = 0; i < 256; i++) {
        //            printf("%x : %d\n", i, R[i]);
        //        }
        unsigned int ignor = coef * (numOfBytes / 3);
        //printf("We should ignore %d bytes\n", ignor);
        int Rmin = getMin(R, ignor);
        int Gmin = getMin(G, ignor);
        int Bmin = getMin(B, ignor);
        int Rmax = getMax(R, ignor);
        int Gmax = getMax(G, ignor);
        int Bmax = getMax(B, ignor);
        amin = min(Rmin, min(Gmin, Bmin));
        amax = max(Rmax, max(Gmax, Bmax));
    } else {
        fprintf(stderr, "Unknown type: %c", type);
        exit(0);
    }
}

int getMin(unsigned int R[256], unsigned int ignor) {
    int min;
    int sum = 0;
    for (int i = 0; i < 256; i++) {
        sum += R[i];
        if (sum > ignor) {
            min = i;
            break;
        }
    }
    return min;
}

int getMax(const unsigned int *R, unsigned int ignor) {
    int max;
    int sum = 0;
    for (int i = 255; i >= 0; i--) {
        sum += R[i];
        if (sum > ignor) {
            max = i;
            break;
        }
    }
    return max;
}
