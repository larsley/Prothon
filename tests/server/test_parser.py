from unittest import TestCase
import parser


class TestParserParameters(TestCase):
    def setUp(self):
        self.static_parser = parser.Parser({}, {})

    def assertParser(self, args):
        args_str = str(args)
        args_str_no_space = args_str.replace(" ", "")
        self.assertEqual(self.static_parser.parse_parameters(args_str), args)
        self.assertEqual(self.static_parser.parse_parameters(args_str_no_space), args)

    def test_parse_parameters_numbers(self):
        args = ([1, 2], {})
        self.assertParser(args)

    def test_parse_parameters_string(self):
        args = (["1,2"], {})
        self.assertParser(args)
