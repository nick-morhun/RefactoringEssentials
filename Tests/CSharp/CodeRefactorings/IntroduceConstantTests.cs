using RefactoringEssentials.CSharp.CodeRefactorings;
using Xunit;

namespace RefactoringEssentials.Tests.CSharp.CodeRefactorings
{
    // String numerals won't work!
    public class IntroduceConstantTests : CSharpCodeRefactoringTestBase
    {
        [Fact]
        public void TestLocalConstant()
        {
            Test<IntroduceConstantAction>(@"class TestClass
{
    public void Hello ()
    {
        System.Console.WriteLine ($'A');
    }
}", @"class TestClass
{
    public void Hello ()
    {
        const char CharConst = 'A';
        System.Console.WriteLine (CharConst);
    }
}");
        }

        [Fact]
        public void TestLocalConstantHexNumber()
        {
            Test<IntroduceConstantAction>(@"class TestClass
{
    public void Hello ()
    {
        System.Console.WriteLine ($0xAFFE);
    }
}", @"class TestClass
{
    public void Hello ()
    {
        const int Int32Const = 0xAFFE;
        System.Console.WriteLine (Int32Const);
    }
}");
        }

        [Fact]
        public void TestFieldConstant()
        {
            Test<IntroduceConstantAction>(@"class TestClass
{
    public void Hello ()
    {
        System.Console.WriteLine ($'A');
    }
}", @"class TestClass
{
    const char CharConst = 'A';

    public void Hello ()
    {
        System.Console.WriteLine (CharConst);
    }
}", 1);
        }

        [Fact]
        public void TestFieldConstantReplaceAll()
        {
            Test<IntroduceConstantAction>(@"class TestClass
{
    public void Hello (char p = 'A')
    {
        System.Console.WriteLine ('A');
        System.Console.WriteLine ($'A');
    }

    private char c = 'A';
}", @"class TestClass
{
    const char CharConst = 'A';

    public void Hello (char p = CharConst)
    {
        System.Console.WriteLine (CharConst);
        System.Console.WriteLine (CharConst);
    }

    private char c = CharConst;
}", 3);
        }

        [Fact]
        public void TestLocalConstantReplaceAll()
        {
            Test<IntroduceConstantAction>(@"class TestClass
{
    public void Hello ()
    {
        System.Console.WriteLine ('A');
        System.Console.WriteLine ($'A');
        System.Console.WriteLine ('A');
    }
}", @"class TestClass
{
    public void Hello ()
    {
        const char CharConst = 'A';
        System.Console.WriteLine (CharConst);
        System.Console.WriteLine (CharConst);
        System.Console.WriteLine (CharConst);
    }
}", 1);
        }

        [Fact]
        public void TestAlreadyConstant()
        {
            TestWrongContext<IntroduceConstantAction>(@"class TestClass
{
    public void Hello ()
    {
        const int i = $0xAFFE;
    }
}");
        }

        [Fact]
        public void TestAlreadyConstantCase2()
        {
            TestWrongContext<IntroduceConstantAction>(@"class TestClass
{
    const int i = $0xAFFE;
}");
        }

        [Fact(Skip="Not implemented!")]
        public void TestIntroduceConstantInInitializer()
        {
            Test<IntroduceConstantAction>(@"class TestClass
{
    readonly int foo = new Foo ($5);
}", @"class TestClass
{
    const int i = 5;
    readonly int foo = new Foo (i);
}");
        }
    }
}
