#include <iostream>
#include <vector>
using namespace std;

//Functor
 struct add_x
 {
   int x;
   add_x(int y):x(y){}
   int operator()(int y)
   {
      return x-y;
   }
 };
 //Function
 int (func)(int x)
 {
    return ++x;
 }
 std::vector<int> vec{1, 2, 3, 4, 5};
 //fill vec with 1 2 3 4 5
 int (*f)(int) = func;//Function pointer

int main()
{

cout << add_x(1)(2) << endl;
    return 0;
}
