#include <iostream>

using namespace std;

class Data {

public:
    int a;
    void update(int arg_a) {
        a = arg_a + 3;
    }
};

class Deriv:
        public Data{
public:
    void update(int arg_a) {
        Data::update(arg_a);
    }
};

//int Data::a = 2;

void test(const int& x) {
    cout << x << endl;
}

int main()
{
// &Data::a;
    Deriv D;
    D.update(2);
//test(Data::a);
//    cout << Data::a << endl;
    return 0;
}
