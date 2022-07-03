#include <iostream>
#include <ctime>
#include <omp.h>

bool IsSimple(int n) {
    for (int j = 3; j * j <= n; j += 2)
        if (n % j == 0)
            return false;
    return true;
}
void WithOpenMP(int n, int num_threads) {
    double start_time = omp_get_wtime();
    omp_set_num_threads(num_threads);
    int answer = 0;

    #pragma omp parallel
    {
        int y = 0;
        #pragma omp for schedule(static)
        //#pragma omp for schedule(dynamic)
        //#pragma omp for schedule(guided)
        for (int num = 3; num < n; num += 2)
            if (IsSimple(num))
                y++;
        #pragma omp atomic
        answer += y;
    }
    /*
    #pragma omp parallel for schedule(static) reduction(+:answer)
    for (int num = 3; num < n; num += 2)
        if (IsSimple(num))
            answer++;
    */
    std::cout<<"Time:"<<omp_get_wtime()  - start_time<<" s"<<"\n";
    std::cout<<"Result:"<<answer<<"\n";
}
main() {
    int n = 10000000;
    /*
    std::cout<<"n = ";
    std::cin>>n;
    */
    for (int j = 0; j < 3; j++) {
        for (int i = 1; i <= 4; i++) {
            std::cout << "With " << i << " threads" << "\n";
            WithOpenMP(n, i);
        }
    }
}

