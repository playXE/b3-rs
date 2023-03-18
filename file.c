#include <stdio.h>
int factorial(int arg0) {
    int v0 = arg0;
    int var6 = v0;
    int v2 = 2;
    int var3 = v2;
    int v4 = 1;
    int v24 = 0;
    int var4 = v24;
    int v32 = var3;
    int var5 = v32;
header:

    int v34 = var4;
    int v22 = v34;

    var4 = v22;

    int v36 = var5;
    var5 = v36;

    int v38 = var3;
    int v39 = var6;

    int v9 = v38 <= v39;
    int var7 = v9;

    int v41 = var7;

    if (v41 == 0) goto end;

    int v42 = var5;
    int v43 = var4;
    int v13 = v42 * v43;
    int v16 = 1;
    int var8 = v16;
    int v45 = var5;
    int v46 = var8;
    int v17 = v45 + v46;

    int v47 = var4;
    var4 = v47;
    int v49 = var5;
    var5 = v49;
    goto header;
end:
    return var4;
}

int main() {
    printf("%d\n", factorial(5));
}