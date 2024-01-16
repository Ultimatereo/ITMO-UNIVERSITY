package info.kgeorgiy.ja.sultanov.implementor;

import info.kgeorgiy.java.advanced.implementor.Impler;
import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.JarImpler;

import java.nio.file.Path;

public class Implementor implements Impler, JarImpler {
    @Override
    public void implement(Class<?> token, Path root) throws ImplerException {

    }

    @Override
    public void implementJar(Class<?> token, Path jarFile) throws ImplerException {

    }
}
