/*
Copyright (C) 2012 Andrew 'the vk' Maraev

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

yamp = (function () {
	function Input(source, position, line, column) {
		line = line || 1;
		column = column || 1;

		this.Line = line;
		this.Column = column;

		this.Source = source;
		this.Position = position;
	};

	Input.prototype.Advance = function() {
		return new Input(this.Source, 
			this.Position + 1, 
			this.Current() == '\n' ? this.Line + 1 : this.Line,
			this.Current() == '\n' ? 1 : this.Column + 1);
	};

	Input.prototype.Current = function() {
		return this.Source[this.Position];
	};

	Input.prototype.AtEnd = function() {
		return this.Position == this.Source.length;
	};
	
	function Success(result, reminder) {
		this.Result = result;
		this.Remainder = reminder;
	};

	function Failure(input, message, expectations) {
		this.Input = input;
		this.Message = message;
		this.Expectations = expectations;
	};

	Failure.prototype.ToString = function() {
		return 'Parsing failure: ' + this.Message + '; expected "' + this.Expectations + '" (Line: ' + this.Input.Line + ', Column: ' + this.Input.Column + ')';
	};

	function IfSuccess(result, next) {
		if (result instanceof Success) {
			return next(result);
		}

		return new Failure(result.Input, result.Message, result.Expectations);
	};

	function IfFailure(result, next) {
		if (result instanceof Success) {
			return result;
		}

		return next(result);
	};

	function Parser(func) {
		this.Func = func;
	};

	Parser.prototype.Parse = function(input) {
		return yamp.Parse(this, input);
	};

	Parser.prototype.Then = function(second) {
		return yamp.Then(this, second);
	};

	Parser.prototype.Many = function() {
		return yamp.Many(this);
	};

	Parser.prototype.XMany = function() {
		return yamp.XMany(this);
	};

	Parser.prototype.AtLeastOnce = function() {
		return yamp.AtLeastOnce(this);
	};

	Parser.prototype.End = function() {
		return yamp.End(this);
	};

	Parser.prototype.Select = function(convert) {
		return yamp.Select(this, convert);
	};

	Parser.prototype.Token = function() {
		return yamp.Token(this);
	};

	Parser.prototype.Text = function() {
		return yamp.Text(this);
	};

	Parser.prototype.Or = function(second) {
		return yamp.Or(this, second);
	};

	Parser.prototype.Named = function(description) {
		return yamp.Named(this, description);
	};

	Parser.prototype.XOr = function(second) {
		return yamp.XOr(this, second);
	};

	Parser.prototype.Once = function() {
		return yamp.Once(this);
	};

	Parser.prototype.Concat = function(second) {
		return yamp.Concat(this, second);
	};

	Parser.prototype.Return = function(value) {
		return yamp.ReturnParser(this, value);
	};

	Parser.prototype.Except = function(except) {
		return yamp.Except(this, expect);
	};

	Parser.prototype.Until = function(until) {
		return yamp.Until(this, until);
	};

	Parser.prototype.Where = function(predicate) {
		return yamp.Where(this, predicate);
	};

	Parser.prototype.SelectMany = function(selector, projector) {
		return yamp.SelectMany(this, selector, projector);
	};

	return {
		/**
		 * Parse input string using specified parser.
		 * If parse was unsucessful throws exception Error with details about error.
		 * @param {Parser} parser Parser.
		 * @param {string} input Input string.
		 * @return {*} Parse result.
		 */
		Parse: function(parser, input) {

			var i = new Input(input, 0);

			var result = parser.Func(i);

			if (result instanceof Success) {
				return result.Result;
			}

			throw new Error(result.ToString());
		},

		/**
		 * Parse a single character matching 'predicate'.
		 * @param {function(string) : boolean} predicate Predicate to match character.
		 * @param {string} description Description of excepted character.
		 * @return {Parse} Parser.
		 */
		CharPredicate: function(predicate, description) {
			return new Parser(function(input) {
				if (!input.AtEnd()) {
					if (predicate(input.Current()))	{
						return new Success(input.Current(), input.Advance());
					}
					return new Failure(input, 'Unexpected "' + input.Current() + '"', description);
				}
				return new Failure(input, 'Unexpected end of input.', null);
			});
		},

		/**
		 * Parse a single character equals parameter value.
		 * @param {string} c Expected character.
		 * @return {Parser} Parser.
		 */
		Char: function(c) {
			return yamp.CharPredicate(function (i) { return i == c;}, c);
		},

		/**
		 * Parse a single character except matching 'predicate'.
		 * @param {function(string) : boolean} predicate Predicate to not match character.
		 * @param {string} description Description of unexpected character.
		 * @return {Parser} Parser.
		 */
		CharExceptPredicate: function(predicate, decription) {
			return yamp.CharPredicate(function(c) {
				return !predicate(c);
			}, 'any character except ');
		},

		/** Parse a single character except parameter value.
		 * @param {string} c Unexpected character.
		 * @return {Parser} Parser.
		 */
		CharExcept: function(c) {
			return yamp.CharExceptPredicate(function (i) {return i == c;}, c);
		},

		/**
		 * Parser that matches single character that is any letter.
		 * @return {Parser} Parser.
		 */
		Letter: function() {
			return yamp.CharPredicate(function(c) {
				return /[a-zA-Z]/.test(c);
			}, 'letter');
		},

		/**
		 * Parser that matches single character that is any decimal digit.
		 * @return {Parser} Parser.
		 */
		Digit: function() {
			return yamp.CharPredicate(function(c) {
				return /[0-9]/.test(c);
			}, 'digit');
		},

		/**
		 * Parser that matches single character that is any Unicode digit.
		 * Currently implemented same as parser Digit.
		 * @return {Parser} Parser.
		 */
		Numeric: function() {
			return yamp.Digit();
		},
		
		Number: function() {
			return yamp.Text(yamp.AtLeastOnce(yamp.Numeric()));
		},

		Decimal: function() {
			return yamp.SelectMany([
					yamp.Number(),
					yamp.XOr(
						yamp.Then(yamp.Char('.'), function(point) {
							return yamp.Select(yamp.Number(), function(n) {
								return '.' + n;
							});
						}),
						yamp.Return('')
					)
				], function(integral, fraction) {
					return integral + fraction;
				});
		},

		/**
		 * Parser that matches single character that is any letter or decimal digit.
		 * @return {Parser} Parser.
		 */
		LetterOrDigit: function() {
			return yamp.CharPredicate(function(c) {
				return /[a-zA-Z0-9]/.test(c);
			}, 'letter or digit');
		},

		/**
		 * Parser that matches single character that is whitespace.
		 * @return {Parser} Parser.
		 */
		Whitespace: function() {
			return yamp.CharPredicate(function(c) {
				return /[\s]/.test(c);
			});
		},

		/**
		 * Parse specified string from input.
		 * @param {string} string Expected string.
		 * @return {Parser} Parser.
		 */
		String: function(string) {
			var result = yamp.Return([]);
			for (var i = 0; i < string.length; ++i) {
				var cp = yamp.Char(string[i]);
				result = yamp.Concat(result, yamp.Once(cp));
			}

			return yamp.Named(result, string);
		},

		/**
		 * Return parser that call first parser, than, if successful, call second parser.
		 * @param {Parser} first First parser.
		 * @param {function(*) : Parser} second Functor that concats output of first parser and returns second parser.
		 * @return {Parser} Parser.
		 */
		Then: function(first, second) {
			return new Parser(function(input) {
				var r = first.Func(input);
				return IfSuccess(r, function(s) {
					return second(s.Result).Func(s.Remainder);
				});
			});
		},

		/**
		 * Parse stream that contains 0 .. * elements.
		 * @param {Parser} parser Parser for expected elements.
		 * @return {Parser} Parser.
		 */
		Many: function(parser) {
			return new Parser(function(i) {
				var remainder = i;
				var result = [];
				var r = parser.Func(i);
				while (r instanceof Success) {
					if (remainder == r.Remainder) {
						break;
					}

					result.push(r.Result);
					remainder = r.Remainder;
					r = parser.Func(remainder);
				}

				return new Success(result, remainder);
			});
		},

		XMany: function(parser) {
			return yamp.Then(yamp.Many(parser), function(m) {
				return yamp.XOr(yamp.Once(parser), yamp.Return(m));
			});
		},

		/**
		 * Parse stream that contains 1 .. * elements.
		 * @param {Parser} parser Parser for expected elements.
		 * @return {Parser} Parser.
		 */
		AtLeastOnce: function(parser) {
			return yamp.Then(yamp.Once(parser), function(t1) {
				return yamp.Select(yamp.Many(parser), function(ts) {
					return t1.concat(ts);
				});
			});
		},

		/**
		 * Parse end-of-input.
		 * @param {Parser} parser Parser for expected last element in input.
		 * @return {Parser} Parser.
		 */
		End: function(parser) {
			return new Parser(function(i) {
				return IfSuccess(parser.Func(i), function (s) {
					return s.Remainder.AtEnd() ?
						s :
						new Failure(s.Remainder, 
							'unexpected ' + s.Remainder.Current(),
							'end of input');
				});
			});
		},

		 /**
		 * Take the result of first parser and applies function 'convert' to it.
		 * @param {Parser} parser Source parser.
		 * @param {function(*) : Parser} convert Functor to convert result of source parser to different parser.
		 * @return {function(Input): Result} Parser.
		 */
		Select: function(parser, convert) {
			return yamp.Then(parser, function(t) {
				return yamp.Return(convert(t));
			});
		},

		/**
		 * Parse the element, surrounded by any amount of whitespace characters.
		 * @param {Parser} parser Parser for expected element.
		 * @return {Parser} Parser.
		 */
		Token: function(parser) {
			return yamp.Then(
				yamp.Many(yamp.Whitespace()),
				function(leading) {
					return yamp.Then(
						parser,
						function(t) {
							return yamp.Select(yamp.Many(yamp.Whitespace()), function() {return t;});
						}
					);
				}
			);
		},

		/**
		 * Create a parser that indirectly references another parser thus allowing cross-references.
		 * @param {function() : Parser} reference Functor that returns referenced parser.
		 * @return {Parser} Parser.
		 */
		Ref: function(reference) {
			return new Parser(function(i) {
				var p = reference();
				return p.Func(i);
			});
		},

		/**
		 * Takes parser that returns array of characters and creates parser that returns a single string.
		 * @param {Parser} character Source parser.
		 * @return {Parser} Parser.
		 */
		Text: function(characters) {
			return yamp.Select(characters, function(chs) {
				return chs.join('');
			});
		},

		/**
		 * Parse first, if it succeeds, return first, otherwise try second.
		 * @param {Parser} first First parser.
		 * @param {Parser} second Second parser.
		 * @return {Parser} Parser.
		 */
		Or: function(first, second) {
			return new Parser(function(i) {
				var fr = first.Func(i);
				if (fr instanceof Failure) {
					return IfFailure(second.Func(i), function(sf) {
						return new Failure(fr.Input, fr.Message, fr.Expectations + " " + sf.Expectations);
					});
				}

				if (fr.Remainder == i) {
					return IfFailure(second.Func(i), function (sf) {return fr;});
				}

				return fr;
			});
		},

		/**
		 * Names parser to help with error messages.
		 * @param {Parser} parser Parser.
		 * @param {string} description Description of parser.
		 * @return {Parser} Named parser.
		 */
		Named: function(parser, description) {
			return new Parser(function(input) {
				var result = parser.Func(input);
				if (result instanceof Success) {
					return result;
				}
				return new Failure(result.Input, result.Message, description);
			});
		},

		/**
		 * Calls first parser, and, if successful, returns result, otherwise calls second parser.
		 * Assumes first character will determine parser to call.
		 * @param {Parser} first First parser.
		 * @param {Parser} second Second parser.
		 * @return {Parser} Combined parser.
		 */
		XOr: function(first, second) {
			return new Parser(function(i) {
				var fr = first.Func(i);
				if (fr instanceof Failure) {
					if (fr.Input != i) {
						return fr;
					}

					return IfFailure(second.Func(i), function(sf) {
						return new Failure(fr.Input, fr.Message, fr.Expectations + " " + sf.Expectations);
					});
				}

				if (fr.Remainder == i) {
					return IfFailure(second.Func(i), function(sf) {return fr});
				}

				return fr;
			});
		},

		/**
		 * Parse a stream of elements containing only one item.
		 * @param {Parser} parser Parser for expected element.
		 * @return {Parser} Parser.
		 */
		Once: function(parser) {
			return yamp.Select(parser, function(r) {return [r];});
		},

		/**
		 * Create a parser that concats output of two parsers. Both parsers must return parse input into array of elements.
		 * @param {Parser} first First parser.
		 * @param {Parser} second Second parser.
		 * @return {Parser} Combined parser.
		 */
		Concat: function(first, second) {
			return yamp.Then(first, function(f) {
				return yamp.Select(second, function (s) {return f.concat(s);});
			});
		},

		/**
		 * Create a parser that successfully parse any input and return specified value.
		 * @param {*} value Result value.
		 * @return {Parser} Parser.
		 */
		Return: function(value) {
			return new Parser(function(i) { return new Success(value, i);});
		},

		/**
		 * Create a parser that calls specified parser, and, if successful, returns specified value.
		 * @param {Parser} parser Parser.
		 * @param {*} value Result value;
		 */
		ReturnParser: function(parser, value) {
			return yamp.Select(parser, function(t) {return value;});
		},

		Except: function(parser, except) {
			return new Parser(function(i) {
				var r = except.Func(i);
				if (r instanceof Success) {
					return new Failure(i, 'Expected parser succeeded.', 'other than the expected input');
				}

				return parser.Func(i);
			});
		},

		/**
		 * Create a parser that parse input stream with 'parser' until parser 'until' is successful.
		 * @param {Parser} parser Parser for expected elements.
		 * @param {Parser} until Parser for stop condition.
		 * @return {Parser} Parser.
		 */
		Until: function(parser, until) {
			return yamp.Then(yamp.Many(yamp.Except(parser, until)), function(r) {return yamp.ReturnParser(until, r);});
		},

		/**
		 * Create a parser that is sucessful only if parsed value mathes predicate.
		 * @param {Parser} parser Parser for expected element.
		 * @param {function(*) : boolean} predicate Predicate to match.
		 * @return {Parser} Parser.
		 */
		Where: function(parser, predicate) {
			return new Parser(function(i) {
				return IfSuccess(parser.Func(i), function(s) {
					return predicate(s.Result) ?
						s :
						new Failure(i, 'Unexpected ' + s.Result + ".", "");
				});
			});
		},

		/**
		 * Create a parser that combines specified parsers and calls functor 'selector' on results.
		 * @param {Array.<Parser>} parsers Array of parsers.
		 * @param {function (...[*]) : *} selector Functor to combine parsers results to a single result object.
		 * @return {Parser} Parser.
		 */
		SelectMany: function(parsers, selector) {
			return new Parser(function(i) {
				var results = [];
				var input = i;
				for (var pi = 0; pi < parsers.length; ++pi) {
					var r = parsers[pi].Func(input);
					if (r instanceof Failure) {
						return r;
					}
					results.push(r.Result);
					input = r.Remainder;
				}

				return new Success(selector.apply(this, results), input);
			});
		},

		ChainOperator: function(op, operand, apply) {
			return yamp.Then(operand, function(first) {
				return yamp.ChainOperatorRest(first, op, operator, apply);
			});
		},

		ChainOperatorRest: function(firstOperand, op, operand, apply) {
			return yamp.Or(
				yamp.Then(op, function(opvalue) {
					return yamp.Then(operand, function(operandValue) {
						return yamp.ChainOperatorRest(apply(opvalue, firstOperand, operandValue), op, operand, apply);
					})
				}),
				yamp.Return(firstOperand)
			);
		},

		ChainRightOperator: function(op, operand, apply) {
			return yamp.Then(operand, function(first) {
				return yamp.ChainRightOperatorRest(first, op, operand, apply);
			});
		},

		ChainRightOperatorRest: function(lastOperand, op, operand, apply) {
			return yamp.Or(
				 yamp.Then(op, function(opvalue) {
					return yamp.Then(operand, function(operandValue) {
						return yamp.Then(
							yamp.ChainRightOperatorRest(operandValue, op, operand, apply),
							function(r) {
								return yamp.Return(apply(opvalue, lastOperand, r))
							});
					});
				}),
				yamp.Return(lastOperand)
			);
		}
	};
})();
