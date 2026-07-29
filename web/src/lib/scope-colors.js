function hash(str) {
  let h = 0;
  for (let i = 0; i < str.length; i++) {
    h = ((h << 5) - h + str.charCodeAt(i)) | 0;
  }
  return h;
}

export function scopeHue(sym) {
  return ((hash(sym) % 360) + 360) % 360;
}

export function scopeColor(sym) {
  return `hsl(${scopeHue(sym)}, 70%, 42%)`;
}

export function scopeBg(sym) {
  return `hsl(${scopeHue(sym)}, 55%, 92%)`;
}
