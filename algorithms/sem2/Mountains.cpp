#include <cstdio>
#include <vector>
//int tree[2'073'741'823];
//int set[2'073'741'823];
//int numberOfLeaves;
int numberOfElements;
//long long longMax = 1'000'000'000'000'000'007;

using namespace std;

struct Node {
//    int leftBorder;
//    int rightBorder;
    int value;
    int maxPrefix;//Нужна максимальная префиксная сумма, а не просто сумма
//    int id;
    int set;
//    bool isLeaf;
    bool isSet;
    Node* l;
    Node* r;

    Node() {
        l = nullptr;
        r = nullptr;
//        leftBorder = 0;
//        rightBorder = 1'000'000'001;
//        rightBorder = 7;
        value = 0;
        set = 0;
//        id = 0;
//        isLeaf = false;
        isSet = false;
        maxPrefix = 0;
    }
//    Node(int index, int left, int right) {
//        l = nullptr;
//        r = nullptr;
////        leftBorder = left;
////        rightBorder = right;
//        value = 0;
//        set = 0;
//        maxPrefix = 0;
////        id = index;
////        isLeaf = (left == right);
//        isSet = false;
//    }

    void extend(int leftBorder, int rightBorder) {
        if (l == nullptr && (leftBorder != rightBorder)) {
            //int m = leftBorder + (rightBorder - leftBorder)/2;
            l = new Node();
            r = new Node();
        }
   }
    void propagate(int leftBorder, int rightBorder) {
        extend(leftBorder, rightBorder);
        if ((leftBorder != rightBorder) && isSet) {
            l->value = (rightBorder - leftBorder + 1)/2 * set;
            l->set = set;
            l->isSet = true;
            l->maxPrefix = max(l->set, l->value);;
            r->value = (rightBorder - leftBorder + 1)/2 * set;
            r->set = set;
            r->isSet = true;
            r->maxPrefix = max(r->set, r->value);;
            isSet = false;
        }
    }
    int findAnswer(int height, bool info, int leftBorder, int rightBorder) { // answer = -1 в начале
        if (info) {
            printf("The parent:     %12d %12d %12d %12d %12d %12d\n", leftBorder, rightBorder, value,
                   set, maxPrefix, isSet);
        }
        if (leftBorder == rightBorder) {
            return min(rightBorder, numberOfElements);
        }
        int m = leftBorder + (rightBorder - leftBorder)/2;
        if (info) {
            printf("The left child: %12d %12d %12d %12d %12d %12d\n", leftBorder, m, l->value,
                   l->set, l->maxPrefix, l->isSet);
            printf("The right child:%12d %12d %12d %12d %12d %12d\n", m + 1, rightBorder, r->value,
                   r->set, r->maxPrefix, r->isSet);
        }
        if (maxPrefix <= height) {
            return min(rightBorder, numberOfElements);
        }
        //extend();
        propagate(leftBorder, rightBorder);
        if (l->maxPrefix <= height) {
            return r->findAnswer(height - (l->value), info, m + 1, rightBorder);
        }
        return l->findAnswer(height, info, leftBorder, m);
        // Я не ебу, как это по человечески реализовать. Аллах, помоги!
        // Если разница height - tree[v] не меньше нуля и мы были в левом сыне, значит идём в правого сына
        // Если разница не меньше нуля и мы в правом сыне, то мы просто берём правую границу правого сына как ответ
        // Если разница меньше нуля, то идём в левого сына и смотрим его детей (в правого нет смысла идти)
        // Мы должны ещё при конфигурации менять на max(-maxx на промежутке и value_)

    }

    void set_(int l_, int r_, int value_, bool info, int leftBorder, int rightBorder) {
        if (info) {
            printf("Before:     %12d %12d %12d %12d %12d %12d \n", leftBorder, rightBorder, value,
                   set, maxPrefix, isSet);
        }
        if ((r_ < leftBorder) || (l_ > rightBorder)) {
            if (info) {
                printf("After0:     %12d %12d %12d %12d %12d %12d\n", leftBorder, rightBorder, value,
                       set, maxPrefix, isSet);
            }
            return;
        }
        if ((l_ <= leftBorder) && (rightBorder <= r_)) {
            value = value_ * (rightBorder - leftBorder + 1);
            set = value_;
            isSet = true;
            maxPrefix = max(value_, value);
            if (info) {
                printf("After1:     %12d %12d %12d %12d %12d %12d \n", leftBorder, rightBorder, value,
                       set, maxPrefix, isSet);
            }
            return;
        }
        propagate(leftBorder, rightBorder);
        int m = leftBorder + (rightBorder - leftBorder)/2;
        l->set_(l_, r_, value_, info, leftBorder, m);
        r->set_(l_, r_, value_, info, m + 1, rightBorder);
        value = r->value + l->value;
        maxPrefix = max(l->maxPrefix, l->value + r->maxPrefix);
//        if (info) {
//            printf("After3:     %12d %12d %12d %12d %12d %12d %12d %12d\n", id, leftBorder, rightBorder, value,
//                   set, maxPrefix, isSet, isLeaf);
//        }
    }
};
Node* t = new Node();
bool info = false;
int main() {
    scanf("%d", &numberOfElements);
    //printf("%d\n", a);
    int l, r;
    char command;
    int value;
//    numberOfLeaves = 1'073'741'824;
//    for (int i = 0; i < 2'073'741'823; i++) {
//        tree[i] = 0;
//    }
    //SegmentTree tree = new SegmentTree(length, mas);
    while (true) {
        //cout << (scanf("%s", &command));
        scanf("%s", &command);
        if (command == 'Q') {
            //printf("FindAnswer:\n");
            scanf("%d", &value);
            printf("%d\n", t->findAnswer(value, info, 0, 1'073'741'823));
        } else if (command == 'I') {
            //printf("Add:\n");
            scanf("%d %d %d", &l, &r, &value);
            t->set_(l - 1, r - 1, value, info, 0, 1'073'741'823);
        } else {
            //printf("Wait the fuck?\n");
            break;
        }
    }
}

