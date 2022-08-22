package copier

import (
	"errors"
	"fmt"
	"reflect"
	"strconv"
	"strings"
	"time"
)

// TODO: clean up errors

var (
	ErrInvalidCopyDestination        = errors.New("copy destination is invalid")
	ErrInvalidCopySource             = errors.New("copy source is invalid")
	ErrMapKeyNotMatch                = errors.New("map's key type doesn't match")
	ErrNotSupported                  = errors.New("not supported")
	ErrFieldNameTagStartNotUpperCase = errors.New("copier field name tag must be start upper case")
	ErrMissingField                  = errors.New("missing field")

	ErrConversion   = errors.New("failed to convert value")
	ErrTypeMismatch = errors.New("type mismatch")
)

var (
	errInvalidCopyDestinationStr        = "copy destination is invalid"
	errMapKeyNotMatchStr                = "map's key type doesn't match"
	errNotSupportedStr                  = "not supported"
	errFieldNameTagStartNotUpperCaseStr = "copier field name tag must be start upper case"
	errMissingFieldStr                  = "missing field"

	errConversionStr   = "failed to convert value"
	errTypeMismatchStr = "type mismatch"

	timeType    = reflect.TypeOf(time.Time{})
	invalidType = reflect.TypeOf(nil)
)

// Type of tags that the copier understands.
const (
	// tagMust denotes that a value must be copied. If copying fails then the
	// copier will panic. To return an error instead of panicking, combine it
	// with `nopanic` tag.
	tagMust uint8 = 1 << iota

	// tagNoPanic denotes that the program should not panic when the `must` flag
	// is defined and value is not copied. The program will return an error
	// instead.
	tagNoPanic

	// tagIgnore denotes that a value will not be copied.
	tagIgnore
)

// Options for the copier.
type Options struct {
	Converters []TypeConverter

	// ConsiderTags is a list of additional struct tag names to use when copying
	// structs. If you add the `json` here, the struct field name will be taken
	// from the `json` tag, if one is present. By default only the tag `copier`
	// is used.
	ConsiderTags []string

	// TODO: maybe add support for shallow copy?
	// // DeepCopy is a flag to enable deep copying of values.
	// DeepCopy bool

	// IgnoreErrors is a flag to ignore errors and continuing copying of values.
	IgnoreErrors bool

	// IgnoreEmpty is a flag to skip copying of empty values.
	IgnoreEmpty bool

	// CopyUnexported is a flag to copy unexported struct fields.
	CopyUnexported bool

	// AutoConvert is a flag to enable auto conversion of types like numbers,
	// strings, etc.
	AutoConvert bool
}

// TypeConverter defines a custom type conversion from a source type to a
// destination type. When the copier encounters values that exactly match the
// source and destination type, it will use the conversion function `Fn` to
// convert the value. Beware, that the converter doesn't work on unexported
// struct fields.
type TypeConverter struct {
	SrcType interface{}
	DstType interface{}
	Fn      func(src interface{}, copier *Copier) (dst interface{}, stop bool, err error)
}

// converterFunc is a function that converts a value from a source type to a
// destination type.
type converterFunc func(src interface{}, copier *Copier) (dst interface{}, stop bool, err error)

// converterPair is a helper type for fast converter lookup.
type converterPair struct {
	SrcType reflect.Type
	DstType reflect.Type
}

// Copier represents a copy utility that can deep copy any value type and
// performs automatic type conversion.
type Copier struct {
	options Options

	// converter maps for fast converter lookup

	srcMatchConverters    map[converterPair]converterFunc
	dstMatchConverters    map[converterPair]converterFunc
	srcDstMatchConverters map[converterPair]converterFunc
	convertersEnabled     bool

	structFieldsCache map[reflect.Type]map[string]structField
}

// New creates a new Copier with the given options.
func New(options Options) *Copier {
	// create converter maps for fast converter lookup
	srcMatchConverters := make(map[converterPair]converterFunc)
	dstMatchConverters := make(map[converterPair]converterFunc)
	srcDstMatchConverters := make(map[converterPair]converterFunc)
	for _, converter := range options.Converters {
		if converter.SrcType == nil && converter.DstType == nil {
			// invalid, continue
			continue
		}
		if converter.SrcType == nil {
			dstType, ok := converter.DstType.(reflect.Type)
			if !ok {
				dstType = reflect.TypeOf(converter.DstType)
			}
			dstMatchConverters[converterPair{DstType: dstType}] = converter.Fn

		} else if converter.DstType == nil {
			srcType, ok := converter.SrcType.(reflect.Type)
			if !ok {
				srcType = reflect.TypeOf(converter.SrcType)
			}
			srcMatchConverters[converterPair{SrcType: srcType}] = converter.Fn

		} else {
			srcType, ok := converter.SrcType.(reflect.Type)
			if !ok {
				srcType = reflect.TypeOf(converter.SrcType)
			}
			dstType, ok := converter.DstType.(reflect.Type)
			if !ok {
				dstType = reflect.TypeOf(converter.DstType)
			}
			srcDstMatchConverters[converterPair{SrcType: srcType, DstType: dstType}] = converter.Fn
		}
	}
	convertersEnabled := len(srcMatchConverters) > 0 || len(dstMatchConverters) > 0 || len(srcDstMatchConverters) > 0

	// create cache for struct fields info
	cache := make(map[reflect.Type]map[string]structField)

	return &Copier{
		options:               options,
		srcMatchConverters:    srcMatchConverters,
		dstMatchConverters:    dstMatchConverters,
		srcDstMatchConverters: srcDstMatchConverters,
		convertersEnabled:     convertersEnabled,

		structFieldsCache: cache,
	}
}

// Copy creates a deep copy of whatever is passed to it.
func (c *Copier) Copy(src interface{}) (interface{}, error) {
	if src == nil {
		return nil, nil
	}
	srcVal := reflect.ValueOf(src)
	_, _, dstTopVal := initDstVal(reflect.Value{}, srcVal.Type())
	dstVal := dstTopVal.Elem()
	if err := c.copyRecursive(srcVal, dstVal); err != nil {
		err.bstTyp = srcVal.Type()
		return nil, err
	}
	return dstVal.Interface(), nil
}

// CopyTo deep copies the src object to dst.
func (c *Copier) CopyTo(src interface{}, dst interface{}) error {
	if src == nil {
		return nil
	}
	srcVal := reflect.ValueOf(src)
	dstVal := reflect.ValueOf(dst)
	if err := c.copyRecursive(srcVal, dstVal); err != nil {
		err.bstTyp = srcVal.Type()
		return err
	}
	return nil
}

// initDstVal returns the zero value for dst if dst is not initialized. If dst
// is invalid, the type of src will be used to initialize it.
func initDstVal(dstVal reflect.Value, srcType reflect.Type) (baseVal, basePtr, topPtr reflect.Value) {
	// if dst value is invalid, a new value is created based on the src value
	if !dstVal.IsValid() {
		// don't know what to create -> return nil
		if srcType == invalidType {
			return reflect.Value{}, reflect.Value{}, reflect.Value{}
		}

		// new value based on srcType
		dstVal = reflect.New(srcType).Elem()
	}

	// if dst value is of type pointer or interface, it has to be initialized
	dstType := dstVal.Type()
	switch dstVal.Kind() {
	case reflect.Pointer:
		// count indirections and get last indirected type
		drefVal := dstVal
		drefType := drefVal.Type()
		// alreadyInitialized := true
		indCount := uint8(0)
		for drefType.Kind() == reflect.Pointer {
			if !drefVal.IsNil() {
				drefVal = drefVal.Elem()
			}
			drefType = drefType.Elem()
			indCount++
		}

		// dstVal points to something valid (like an already initialized value)
		if drefVal.Kind() != reflect.Pointer && drefVal.IsValid() {
			// need to go deeper to find out what is contained in interface
			if drefVal.Kind() == reflect.Interface {
				tmpVal, tmpPtr, tmpTopPtr := initDstVal(drefVal, srcType)
				if tmpTopPtr.IsValid() {
					drefVal.Set(tmpTopPtr.Elem())
				}
				basePtr = tmpPtr
				baseVal = tmpVal
				return baseVal, basePtr, topPtr
			}

			// dstVal points to an already initialized value -> return as is
			return drefVal, drefVal.Addr(), reflect.Value{}
		}

		// initialize value and chain of pointers
		basePtr = reflect.New(drefType)
		baseVal = basePtr.Elem()
		topPtr = basePtr
		// lastUpperPtr := basePtr

		for i := uint8(0); i < indCount; i++ {
			// create indirected value
			tmpPtr := reflect.New(topPtr.Type())
			// set indirected value
			tmpPtr.Elem().Set(topPtr)

			topPtr = tmpPtr
		}

		// if it is an interface, try to resolve it to a concrete type in the
		// next step, otherwise just return the newly created value
		if baseVal.Kind() == reflect.Interface {
			tmpVal, tmpPtr, tmpTopPtr := initDstVal(baseVal, srcType)
			if tmpTopPtr.IsValid() {
				baseVal.Set(tmpTopPtr.Elem())
			}
			basePtr = tmpPtr
			baseVal = tmpVal
			return baseVal, basePtr, topPtr
		}

		return

	case reflect.Interface:
		// if dst contains something non-nil, go deeper
		if !dstVal.IsNil() {
			topPtr = basePtr
			tmpVal, tmpPtr, tmpTopPtr := initDstVal(dstVal.Elem(), srcType)
			if tmpTopPtr.IsValid() {
				dstVal.Set(tmpTopPtr.Elem())
			}
			basePtr = tmpPtr
			baseVal = tmpVal
			return baseVal, basePtr, topPtr
		}

		baseVal = dstVal

		// if the source type implements the interface, use the type of the source
		if srcType.Implements(dstType) {
			basePtr = reflect.New(srcType)
			topPtr = basePtr
			baseVal = basePtr.Elem()

			// if it is a pointer, resolve the indirection and create the
			// concrete value
			if baseVal.Kind() == reflect.Pointer {
				baseVal, basePtr, _ = initDstVal(baseVal, srcType)
			}

			return baseVal, basePtr, topPtr
		}

		// can't create a specific type, so just return the interface type
		// as is
		return
	}

	// for any other type just return the value
	baseVal = dstVal
	if baseVal.CanAddr() {
		basePtr = dstVal.Addr()
		topPtr = basePtr
	}

	return
}

// copyRecursive does the actual copying of the interface. It currently has
// limited support for what it can handle. Add as needed.
func (c *Copier) copyRecursive(srcVal, dstVal reflect.Value) *CopierError {
	// check for invalid values
	if !srcVal.IsValid() {
		return newError("copy source is invalid")
	}

	if c.convertersEnabled {
		// try to use custom converters
		if stop, err := c.copyWithConverters(srcVal, dstVal); err != nil {
			return wrapError(err, "copy via custom converter failed")
		} else if stop {
			return nil
		}
	}

	// handle according to original's Kind
	switch srcVal.Kind() {
	// -------------------------------------------------------------------------
	// Pointer ➜ Any
	// -------------------------------------------------------------------------
	case reflect.Pointer:
		// resolve src pointer
		srcElmVal := srcVal.Elem()
		// if it isn't valid, return
		if !srcElmVal.IsValid() {
			return nil
		}
		// continue recursive copy
		return c.copyRecursive(srcElmVal, dstVal)

	// -------------------------------------------------------------------------
	// Interface ➜ Any
	// -------------------------------------------------------------------------
	case reflect.Interface:
		// stop, if src is nil
		if srcVal.IsNil() {
			return nil
		}
		// get concrete value
		srcElmVal := srcVal.Elem()
		// if it isn't valid, return
		if !srcElmVal.IsValid() {
			return nil
		}
		// continue recursive copy
		return c.copyRecursive(srcElmVal, dstVal)
	}

	// stop if empty src values shall be ignored
	if c.options.IgnoreEmpty && srcVal.IsZero() {
		return nil
	}

	// initialize dst value and resolve pointers
	dstBaseVal, _, dstTopPtr := initDstVal(dstVal, srcVal.Type())
	if !dstBaseVal.IsValid() {
		return newError(errInvalidCopyDestinationStr)
	}

	switch srcVal.Kind() {
	// -------------------------------------------------------------------------
	// Struct ➜ Struct, Map
	// -------------------------------------------------------------------------
	case reflect.Struct:
		switch dstBaseVal.Kind() {
		case reflect.Struct:
			if c.options.CopyUnexported {
				copyUnexported(srcVal, dstBaseVal)
			}

			// treat time.Time differently (force copy unexported fields)
			if srcVal.Type() == timeType && dstBaseVal.Type() == timeType {
				copyUnexported(srcVal, dstBaseVal)
			}

			// get a list of exported fields for src and dst
			srcFields := c.getStructFields(srcVal)
			dstFields := c.getStructFields(dstBaseVal)

			// copy fields from src to dst
			for k, dstFld := range dstFields {
				srcFld, ok := srcFields[k]
				if !ok {
					if (dstFld.Flags & tagMust) != 0 {
						err := newError("missing field '%s'", k)
						err.key = append(err.key, k)
						if (dstFld.Flags & tagNoPanic) != 0 {
							return err
						}
						panic(err)
					}
					continue
				}
				err := c.copyRecursive(srcVal.Field(srcFld.Index), dstBaseVal.Field(dstFld.Index))
				if err != nil {
					err.key = append(err.key, k)
					return err
				}
			}

		case reflect.Map:
			if dstBaseVal.IsNil() {
				dstBaseVal.Set(reflect.MakeMap(srcVal.Type()))
			}
			// get a list of exported fields for src
			srcFields := c.getStructFields(srcVal)

			// copy fields from src to dst
			for key, srcFld := range srcFields {
				keyVal := reflect.ValueOf(key)
				dstKeyType := dstBaseVal.Type().Key()
				dstValType := dstBaseVal.Type().Elem()

				// copy the key
				newKeyVal := reflect.New(dstKeyType).Elem()
				if err := c.copyRecursive(keyVal, newKeyVal); err != nil {
					err.key = append(err.key, fmt.Sprint(key))
					return err
				}

				// create a new value for the key
				dstElmVal := dstBaseVal.MapIndex(newKeyVal)
				if !dstElmVal.IsValid() {
					dstElmVal = reflect.New(dstValType).Elem()
				}

				// copy the value
				srcElmVal := srcVal.Field(srcFld.Index)
				err := c.copyRecursive(srcElmVal, dstElmVal)
				if err != nil {
					err.key = append(err.key, fmt.Sprint(key))
					return err
				}

				// add key-value-pair to destination map
				dstBaseVal.SetMapIndex(newKeyVal, dstElmVal)
			}

		default:
			return newError("incompatible types (%s ➜ %s)", srcVal.Type(), dstVal.Type())
		}

	// -------------------------------------------------------------------------
	// Slice ➜ Slice
	// -------------------------------------------------------------------------
	case reflect.Slice:
		if srcVal.IsNil() {
			return nil
		}
		switch dstBaseVal.Kind() {
		case reflect.Slice:
			if dstBaseVal.IsNil() {
				// make a new slice
				if !dstBaseVal.CanSet() {
					return newError(errInvalidCopyDestinationStr)
				}
				dstBaseVal.Set(reflect.MakeSlice(dstBaseVal.Type(), srcVal.Len(), srcVal.Cap()))
			}
			// copy elements up to the length of the shorter slice
			srcInd := 0
			for srcInd < srcVal.Len() && srcInd < dstBaseVal.Len() {
				err := c.copyRecursive(srcVal.Index(srcInd), dstBaseVal.Index(srcInd))
				if err != nil {
					err.key = append(err.key, fmt.Sprintf("[%v]", srcInd))
					return err
				}
				srcInd++
			}
			// if the source slice is longer (if it was already initialized), append
			// elements to the destination slice
			res := dstBaseVal
			dstValType := dstBaseVal.Type().Elem()
			for srcInd < srcVal.Len() {
				_, _, cpTopVal := initDstVal(reflect.Value{}, dstValType)
				cpVal := cpTopVal.Elem()
				err := c.copyRecursive(srcVal.Index(srcInd), cpVal)
				if err != nil {
					err.key = append(err.key, fmt.Sprintf("[%v]", srcInd))
					return err
				}
				res = reflect.Append(res, cpVal)
				srcInd++
			}
			// slice might have changed, so set the new value
			if res != dstBaseVal {
				if !dstBaseVal.CanSet() {
					return newError("cannot append new values to slice")
				}
				dstBaseVal.Set(res)
			}

		default:
			return newError("incompatible types (%s ➜ %s)", srcVal.Type(), dstVal.Type())
		}

	// -------------------------------------------------------------------------
	// Map ➜ Map, Struct
	// -------------------------------------------------------------------------
	case reflect.Map:
		if srcVal.IsNil() {
			return nil
		}
		switch dstBaseVal.Kind() {
		case reflect.Map:
			if dstBaseVal.IsNil() {
				dstBaseVal.Set(reflect.MakeMap(srcVal.Type()))
			}
			for _, key := range srcVal.MapKeys() {
				dstKeyType := dstBaseVal.Type().Key()
				dstValType := dstBaseVal.Type().Elem()

				// copy the key
				newKeyVal := reflect.New(dstKeyType).Elem()
				if err := c.copyRecursive(key, newKeyVal); err != nil {
					err.key = append(err.key, fmt.Sprint(key.Interface()))
					return err
				}

				// create a new value for the key
				dstElmVal := dstBaseVal.MapIndex(newKeyVal)
				if !dstElmVal.IsValid() {
					dstElmVal = reflect.New(dstValType).Elem()
				}

				// copy the value
				srcElmVal := srcVal.MapIndex(key)
				err := c.copyRecursive(srcElmVal, dstElmVal)
				if err != nil {
					err.key = append(err.key, fmt.Sprint(key.Interface()))
					return err
				}

				// add key-value-pair to destination map
				dstBaseVal.SetMapIndex(newKeyVal, dstElmVal)
			}

		case reflect.Struct:
			// get a list of exported fields for dst
			dstFields := c.getStructFields(dstBaseVal)

			// copy fields from src to dst
			for key, dstFld := range dstFields {
				srcElmVal := srcVal.MapIndex(reflect.ValueOf(key))
				if !srcElmVal.IsValid() {
					if (dstFld.Flags & tagMust) != 0 {
						err := newError("missing field '%s'", key)
						err.key = append(err.key, key)
						if (dstFld.Flags & tagNoPanic) != 0 {
							return err
						}
						panic(err)
					}
					continue
				}
				err := c.copyRecursive(srcElmVal, dstBaseVal.Field(dstFld.Index))
				if err != nil {
					err.key = append(err.key, key)
					return err
				}
			}

		default:
			return newError("incompatible types (%s ➜ %s)", srcVal.Type(), dstVal.Type())
		}

	// -------------------------------------------------------------------------
	// Primitive ➜ Primitive
	// -------------------------------------------------------------------------
	default:
		if err := c.copyPrimitive(srcVal, dstBaseVal); err != nil {
			return err
		}
	}

	// update destination value
	// TODO: is there another case we need to handle?
	if dstTopPtr.IsValid() {
		if !dstVal.CanSet() {
			return newError(errInvalidCopyDestinationStr)
		}
		dstVal.Set(dstTopPtr.Elem())
	}

	return nil
}

// Tag Flags
type structTagInfo struct {
	Name  string
	Flags uint8
}

// Tag Flags
type structField struct {
	Name  string
	Flags uint8
	Index int
}

// getStructFields returns the (filtered) fields of the given struct.
func (c *Copier) getStructFields(structVal reflect.Value) map[string]structField {
	typ := structVal.Type()
	if cached, ok := c.structFieldsCache[typ]; ok {
		return cached
	}

	flds := make([]reflect.StructField, 0, structVal.NumField())
	for i := 0; i < structVal.NumField(); i++ {
		// the struct field `PkgPath` is empty for exported fields
		if structVal.Type().Field(i).PkgPath != "" {
			continue
		}
		flds = append(flds, structVal.Type().Field(i))
	}

	// get the struct tag info
	tagInfos := c.getStructTagInfo(structVal.Type(), flds)

	// create a list of fields with associated tag info
	structFlds := make(map[string]structField, len(flds))
	for _, fld := range flds {
		tagInfo := tagInfos[fld.Name]
		if (tagInfo.Flags & tagIgnore) != 0 {
			continue
		}
		mappedName := tagInfo.Name
		if mappedName == "" {
			mappedName = fld.Name
		}
		structFlds[mappedName] = structField{
			Name:  fld.Name,
			Flags: tagInfo.Flags,
			Index: fld.Index[0],
		}
	}

	// cache fields info
	c.structFieldsCache[typ] = structFlds

	return structFlds
}

func (c *Copier) getStructTagInfo(typ reflect.Type, flds []reflect.StructField) map[string]structTagInfo {
	// struct field name -> parsed tag info
	info := make(map[string]structTagInfo)

	// parse tag info for each field
	for _, fld := range flds {
		tag := fld.Tag.Get("copier")
		if tag != "" {
			info[fld.Name] = parseTag(tag)
			continue
		}
		for _, tagName := range c.options.ConsiderTags {
			tag = fld.Tag.Get(tagName)
			if tag != "" {
				info[fld.Name] = parseTag(tag)
				break
			}
		}
	}

	return info
}

// parseTag parses a tag string into a name and flags.
func parseTag(tag string) (info structTagInfo) {
	for _, t := range strings.Split(tag, ",") {
		switch t {
		case "-":
			info.Flags = tagIgnore
			return
		case "must":
			info.Flags = info.Flags | tagMust
		case "nopanic":
			info.Flags = info.Flags | tagNoPanic
		default:
			if info.Name == "" {
				info.Name = strings.TrimSpace(t)
			}
		}
	}
	return
}

// copyWithConverters uses the provided custom converters to copy the value.
func (c *Copier) copyWithConverters(srcVal, dstVal reflect.Value) (stop bool, err error) {
	srcType := srcVal.Type()
	dstType := dstVal.Type()

	// match srcVal and dstVal by kind
	if len(c.srcDstMatchConverters) > 0 {
		pair := converterPair{SrcType: srcType, DstType: dstType}
		if conv, ok := c.srcDstMatchConverters[pair]; ok {
			stop, err := c.applyConverter(conv, srcVal, dstVal)
			if err != nil {
				return false, err
			}
			return stop, nil
		}
	}

	// match srcVal by kind
	if len(c.srcMatchConverters) > 0 {
		pair := converterPair{SrcType: srcType}
		if conv, ok := c.srcMatchConverters[pair]; ok {
			stop, err := c.applyConverter(conv, srcVal, dstVal)
			if err != nil {
				return false, err
			}
			return stop, nil
		}
	}

	// match dstVal by kind
	if len(c.dstMatchConverters) > 0 {
		pair := converterPair{DstType: dstType}
		if conv, ok := c.dstMatchConverters[pair]; ok {
			stop, err := c.applyConverter(conv, srcVal, dstVal)
			if err != nil {
				return false, err
			}
			return stop, nil
		}
	}

	return
}

// applyConverter applies the converter to convert/copy the value.
func (c *Copier) applyConverter(conv converterFunc, srcVal, dstVal reflect.Value) (bool, error) {
	result, stop, err := conv(srcVal.Interface(), c)
	if err != nil {
		return false, err
	}
	if result != nil {
		defer func() {
			if r := recover(); r != nil {
				err = newError("failed to set converted value: %s", r)
			}
		}()
		dstVal.Set(reflect.ValueOf(result))
	}
	return stop, nil
}

// copyUnexported copies the unexported fields of a struct value.
func copyUnexported(srcVal, dstVal reflect.Value) {
	if srcVal.Kind() != reflect.Struct || dstVal.Kind() != reflect.Struct || !srcVal.Type().AssignableTo(dstVal.Type()) {
		return
	}

	// create a shallow copy of dstVal and set all values (including unexported)
	// from srcVal to it
	tmp := indirectValue(reflect.New(dstVal.Type()))
	tmp.Set(srcVal)

	// revert exported fields
	for i := 0; i < dstVal.NumField(); i++ {
		if tmp.Field(i).CanSet() {
			tmp.Field(i).Set(dstVal.Field(i))
		}
	}
	dstVal.Set(tmp)
}

// copyPrimitive copies and converts a primitive value.
func (c *Copier) copyPrimitive(srcVal, dstVal reflect.Value) *CopierError {
	// fast path if no auto conversion is desired
	if !c.options.AutoConvert {
		if srcVal.Kind() != dstVal.Kind() {
			return newError("type mismatch (%s != %s)", srcVal.Type(), dstVal.Type())
		}
		dstVal.Set(srcVal)
		return nil
	}

	// auto conversion
	switch dstVal.Kind() {
	case reflect.String:
		switch srcVal.Kind() {
		case reflect.String:
			dstVal.SetString(srcVal.String())
		case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
			dstVal.SetString(strconv.FormatInt(srcVal.Int(), 10))
		case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
			dstVal.SetString(strconv.FormatUint(srcVal.Uint(), 10))
		case reflect.Float32, reflect.Float64:
			dstVal.SetString(strconv.FormatFloat(srcVal.Float(), 'f', -1, 64))
		case reflect.Complex64, reflect.Complex128:
			dstVal.SetString(strconv.FormatComplex(srcVal.Complex(), 'f', -1, 128))
		case reflect.Bool:
			if srcVal.Bool() {
				dstVal.SetString("true")
			} else {
				dstVal.SetString("false")
			}
		}
		// convert all other types using the fmt package
		dstVal.SetString(srcVal.String())

	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		switch srcVal.Kind() {
		case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
			dstVal.SetInt(srcVal.Int())
		case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
			dstVal.SetInt(int64(srcVal.Uint()))
		case reflect.Float32, reflect.Float64:
			dstVal.SetInt(int64(srcVal.Float()))
		case reflect.Complex64, reflect.Complex128:
			dstVal.SetInt(int64(real(srcVal.Complex())))
		case reflect.Bool:
			if srcVal.Bool() {
				dstVal.SetInt(1)
			} else {
				dstVal.SetInt(0)
			}
		case reflect.String:
			i, err := strconv.ParseInt(srcVal.String(), 10, 64)
			if err != nil {
				return newError("conversion failed (string ➜ int)")
			}
			dstVal.SetInt(i)
		default:
			return newError("unsupported conversion (%s ➜ %s)", srcVal.Type(), dstVal.Type())
		}

	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		switch srcVal.Kind() {
		case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
			dstVal.SetUint(uint64(srcVal.Int()))
		case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
			dstVal.SetUint(srcVal.Uint())
		case reflect.Float32, reflect.Float64:
			dstVal.SetUint(uint64(srcVal.Float()))
		case reflect.Complex64, reflect.Complex128:
			dstVal.SetUint(uint64(real(srcVal.Complex())))
		case reflect.Bool:
			if srcVal.Bool() {
				dstVal.SetUint(1)
			} else {
				dstVal.SetUint(0)
			}
		case reflect.String:
			i, err := strconv.ParseUint(srcVal.String(), 10, 64)
			if err != nil {
				return newError("conversion failed (string ➜ uint)")
			}
			dstVal.SetUint(i)
		default:
			return newError("unsupported conversion (%s ➜ %s)", srcVal.Type(), dstVal.Type())
		}

	case reflect.Float32, reflect.Float64:
		switch srcVal.Kind() {
		case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
			dstVal.SetFloat(float64(srcVal.Int()))
		case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
			dstVal.SetFloat(float64(srcVal.Uint()))
		case reflect.Float32, reflect.Float64:
			dstVal.SetFloat(srcVal.Float())
		case reflect.Complex64, reflect.Complex128:
			dstVal.SetFloat(float64(real(srcVal.Complex())))
		case reflect.Bool:
			if srcVal.Bool() {
				dstVal.SetFloat(1)
			} else {
				dstVal.SetFloat(0)
			}
		case reflect.String:
			f, err := strconv.ParseFloat(srcVal.String(), 64)
			if err != nil {
				return newError("conversion failed (string ➜ float)")
			}
			dstVal.SetFloat(f)
		default:
			return newError("unsupported conversion (%s ➜ %s)", srcVal.Type(), dstVal.Type())
		}

	case reflect.Bool:
		switch srcVal.Kind() {
		case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
			dstVal.SetBool(srcVal.Int() != 0)
		case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
			dstVal.SetBool(srcVal.Uint() != 0)
		case reflect.Float32, reflect.Float64:
			dstVal.SetBool(srcVal.Float() != 0.0)
		case reflect.Complex64, reflect.Complex128:
			dstVal.SetBool(real(srcVal.Complex()) != 0.0)
		case reflect.Bool:
			dstVal.SetBool(srcVal.Bool())
		case reflect.String:
			b, err := strconv.ParseBool(srcVal.String())
			if err != nil {
				return newError("conversion failed (string ➜ bool)")
			}
			dstVal.SetBool(b)
		default:
			return newError("unsupported conversion (%s ➜ %s)", srcVal.Type(), dstVal.Type())
		}

	case reflect.Complex64, reflect.Complex128:
		switch srcVal.Kind() {
		case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
			dstVal.SetComplex(complex(float64(srcVal.Int()), 0))
		case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
			dstVal.SetComplex(complex(float64(srcVal.Uint()), 0))
		case reflect.Float32, reflect.Float64:
			dstVal.SetComplex(complex(srcVal.Float(), 0))
		case reflect.Complex64, reflect.Complex128:
			dstVal.SetComplex(srcVal.Complex())
		case reflect.Bool:
			if srcVal.Bool() {
				dstVal.SetComplex(complex(1, 0))
			} else {
				dstVal.SetComplex(complex(0, 0))
			}
		case reflect.String:
			c, err := strconv.ParseComplex(srcVal.String(), 128)
			if err != nil {
				return newError("conversion failed (string ➜ complex)")
			}
			dstVal.SetComplex(c)
		default:
			return newError("unsupported conversion (%s ➜ %s)", srcVal.Type(), dstVal.Type())
		}

	default:
		return newError("unsupported conversion (%s ➜ %s)", srcVal.Type(), dstVal.Type())
	}

	return nil
}

func indirectValue(val reflect.Value) reflect.Value {
	for val.Kind() == reflect.Pointer {
		val = val.Elem()
	}
	return val
}

func indirectType(typ reflect.Type) (_ reflect.Type, isPtr bool) {
	for typ.Kind() == reflect.Pointer || typ.Kind() == reflect.Slice {
		typ = typ.Elem()
		isPtr = true
	}
	return typ, isPtr
}

// -----------------------------------------------------------------------------
//
// Convenience Functions
//
// -----------------------------------------------------------------------------

// Copy creates a deep copy of whatever is passed to it. If you intend to use
// Copy a bunch of times, then rather create a new Copier instance and use that.
func Copy(src interface{}, options ...Options) (interface{}, error) {
	if options == nil {
		options = []Options{Options{}}
	}
	copier := New(options[0])
	return copier.Copy(src)
}

// CopyTo deep copies the src object to dst. If you intend to use CopyTo a bunch
// of times, then rather create a new Copier instance and use that.
func CopyTo(src interface{}, dst interface{}, options ...Options) error {
	if options == nil {
		options = []Options{Options{}}
	}
	copier := New(options[0])
	return copier.CopyTo(src, dst)
}
