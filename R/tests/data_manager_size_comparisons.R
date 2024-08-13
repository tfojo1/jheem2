

load('../../cached/surveillance.manager.rdata')
SURVEILLANCE.MANAGER = load.data.manager('../../cached/surveillance.manager.rdata')

orig.elem.size = sapply(surveillance.manager, object.size)
new.elem.size = sapply(SURVEILLANCE.MANAGER, object.size)

delta.size = new.elem.size - orig.elem.size; delta.size = sort(delta.size, decreasing = T); delta.size
size.ratio = new.elem.size / orig.elem.size; size.ratio = size.ratio[names(delta.size)]; size.ratio

orig.size = sum(orig.elem.size)
new.size = sum(new.elem.size)
extra.size.mb = (new.size - orig.size) / 1000000; extra.size.mb
extra.size.ratio = new.size/orig.size; extra.size.ratio

