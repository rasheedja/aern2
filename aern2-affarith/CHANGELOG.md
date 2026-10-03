# Changelog

## Unreleased

- Identify sine and cosine fallback error terms by their operation and affine
  operand, preventing unrelated inputs with the same range from cancelling.
  Give the negated cosine helper its own operation name and retain cancellation
  for repeated evaluations of the same expression.
